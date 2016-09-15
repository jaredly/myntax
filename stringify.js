
const variants = ['Lident', 'Ldot']

const makeLoc = source => ({
  loc_start: {
    pos_fname: 'hi',
    pos_lnum: 0,
    // TODO fix all this
    pos_cnum: source.startIdx,
    pos_bol: 0,
  },
  loc_end: {
    pos_fname: 'hi',
    pos_lnum: 0,
    // TODO fix all this
    pos_cnum: source.endIdx,
    pos_bol: 0,
  },
  loc_ghost: false,
})

const makeFn = (args, expr, loc) => {
  const label =
    (args[0][2] ? '?' : '') + (args[0][1] || '')
  return {
    pexp_desc: [
      'Pexp_fun',
      label, // label
      args[0][2] && args[0][2] !== '?' ?
        args[0][2] : null, // default
      args[0][0], // pattern
      args.length > 1 ?
        makeFn(args.slice(1), expr, loc) :
        expr
    ],
    pexp_loc: loc,
    pexp_attributes: [],
  }
}

module.exports = {
  Structures(children) {
    return [children.asRe, []]
  },
  Structure(child) {
    return {
      pstr_desc: child.asRe,
      pstr_loc: makeLoc(this.source),
    }
  },

  Args(args, _, lastArg, __) {
    return args.asRe.concat(lastArg.asRe)
  },
  Arg(label, _, expr) {
    return [label.asRe[0] || "", expr.asRe]
  },

  Pattern(child) {
    return {
      ppat_desc: child.asRe,
      ppat_loc: makeLoc(this.source),
      ppat_attributes: [],
    }
  },

  ValueBinding_normal(pat, _, exp) {
    return {
      pvb_pat: pat.asRe,
      pvb_expr: exp.asRe,
      pvb_loc: makeLoc(this.source),
      pvb_attributes: [],
    }
  },

  ArgPatterns(iter, _, last, __) {
    return iter.asRe.concat(last.asRe)
  },

  ArgPattern_full(pat, _, label, optional) {
    const opt = optional.asRe[0]
    return [pat.asRe, label.asRe[0], opt && opt[0]]
  },

  ArgPattern_punned(_, label, optional) {
    const opt = optional.asRe
    return [{
      ppat_desc: [
        'Ppat_var',
        {txt: label.asRe, loc: makeLoc(label.source)},
      ],
      ppat_loc: makeLoc(label.source),
      ppat_attributes: [],
    }, label.asRe, opt && opt[0]]
  },

  ArgOpt_default(_, expr) {
    return expr.asRe
  },

  ValueBinding_fn(name, _, args, __, ___, expr) {
    return {
      pvb_pat: {
        ppat_desc: [
          'Ppat_var',
          {txt: name.asRe, loc: makeLoc(name.source)}
        ],
        ppat_loc: makeLoc(name.source),
        ppat_attributes: [],
      },
      pvb_expr: makeFn(args.asRe, expr.asRe, makeLoc(this.source)),
      pvb_loc: makeLoc(this.source),
      pvb_attributes: [],
    }
  },

  Pstr_value(_, rec, binding1, _2, bindings) {
    return [
      'Pstr_value',
      [
        rec.children.length ? 'Recursive' : 'Nonrecursive'
      ],
      [binding1.asRe].concat(bindings.asRe)
    ]
  },

  LongLoc(child) {
    return {
      txt: child.asRe,
      loc: makeLoc(this.source),
    }
  },

  StrLoc(child) {
    return {
      txt: child.asRe,
      loc: makeLoc(this.source),
    }
  },

  Expression(child) {
    return {
      pexp_desc: child.asRe,
      pexp_loc: makeLoc(this.source),
      pexp_attributes: [],
    }
  },
  Pstr_eval(expression, attributes) {
    return ['Pstr_eval',
      expression.asRe,
      attributes.children[0].children.map(c => c.asRe),
    ]
  },

  Const_int(child) {
    return ['Const_int', +child.asRe]
  },

  Const_string(child) {
    const delim = child.ctorName === 'multilinestring' ? '' : null
    // last arg == delimeter for multiline string
    return ['Const_string', child.asRe.slice(1, -1), delim]
  },

  _nonterminal(children) {
    // this is a variant
    if (this.ctorName.indexOf('_') !== -1 || variants.indexOf(this.ctorName) !== -1) {
      return [this.ctorName].concat(
        children
          .filter(c => c.isNonterminal())
          .map(c => c.asRe)
      )
    }
    if (this.isIteration()) {
      return children.map(c => c.asRe)
    }
    if (this.isLexical()) {
      return this.source.contents
    }
    if (children.length === 1) {
      return children[0].asRe
    }
    console.log(this)
    console.log(this.ctorName)
    throw new Error('Dunno')
  },

  _terminal() {
    return this.source.contents
  }
}

