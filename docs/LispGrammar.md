# Syntax for Lisp.re


This grammar is displayed using a syntax similar to regular expressions.

- `⦅` and `⦆` "thick parenthesis" indicate grouping, to distinguish from parenthesis that are actually part of the grammar
- <code><sup>+</sup></code> allows one or more of the preceeding term or group
- <code><sup>*</sup></code> allows zero or more of the preceeding term or group
- <code><sup>?</sup></code> allows zero or one of the preceeding term or group

Note that this sacrifices a bit of precision in the interest of readability. For the source of truth, read the source code :D


If you're interested, <a href="../grammars/lispGrammar.re">take a look at the grammar definition</a>
### Structure

 Forms that are valid at the top level of a file or module 

| Name | Syntax |
| --- | --- |
| <i>open</i> | <code>(open <a href="#longcap">longCap</a>)</code> |
| <i>def</i> | <code>(def <a href="#pattern">Pattern</a> <a href="#expression">Expression</a>)</code> |
| <i>defn</i> | <code>(defn <a href="#lowerident">lowerIdent</a> <a href="#fnargs">FnArgs</a> <a href="#expression">Expression</a><sup>*</sup>)</code> |
| <i>def_rec</i> | <code>(def-rec <a href="#pattern">Pattern</a> <a href="#expression">Expression</a><sup>+</sup>)</code> |
| <i>type</i> | <code>(type <a href="#typename">TypeName</a> <a href="#typekind">TypeKind</a><sup>+</sup>)</code> |
| <i>module</i> | <code>(module <a href="#capident">capIdent</a> <a href="#structure">Structure</a><sup>+</sup>)</code> |
| <i>module_alias</i> | <code>(module-alias <a href="#capident">capIdent</a> <a href="#longcap">longCap</a>)</code> |
| <i>external</i> | <code>(external <a href="#lowerident">lowerIdent</a> <a href="#coretype">CoreType</a> <a href="#string">string</a><sup>+</sup>)</code> |
| <i>eval</i> | <code><a href="#expression">Expression</a></code> |

### Expression

| Name | Syntax |
| --- | --- |
| <i>ident</i> | <code><a href="#longident">longIdent</a></code> |
| <i>const</i> | <code><a href="#constant">constant</a></code> |
| <i>constructor</i> | <code>(<a href="#longcap">longCap</a> <a href="#expression">Expression</a><sup>+</sup>)</code> |
| <i>empty_constr</i> | <code><a href="#longcap">longCap</a></code> |
| <i>constructor_poly</i> | <code>(`<a href="#capident">capIdent</a> <a href="#expression">Expression</a><sup>+</sup>)</code> |
| <i>empty_poly</i> | <code>`<a href="#capident">capIdent</a></code> |
| <i>attribute</i> | <code>:<a href="#longident">longIdent</a></code> |
| <i>op</i> | <code><a href="#operator">operator</a></code> |
| <i>tuple</i> | <code>(, <a href="#expression">Expression</a> <a href="#expression">Expression</a><sup>+</sup>)</code> |
| <i>array_literal</i> | <code>[\|<a href="#expression">Expression</a><sup>*</sup>\|]</code> |
| <i>list_literal</i> | <code>[<a href="#expression">Expression</a><sup>*</sup> ⦅...<a href="#expression">Expression</a>⦆<sup>?</sup>]</code> |
| <i>object_literal</i> | <code>{⦅...<a href="#expression">Expression</a>⦆<sup>?</sup> <a href="#objectitem">ObjectItem</a><sup>*</sup>}</code> |
| <i>let</i> | <code>(let [<a href="#pattern">Pattern</a> <a href="#expression">Expression</a><sup>+</sup>] <a href="#expression">Expression</a><sup>*</sup>)</code> |
| <i>do</i> | <code>(do <a href="#expression">Expression</a><sup>*</sup>)</code> |
| <i>assert</i> | <code>(assert <a href="#expression">Expression</a>)</code> |
| <i>lazy</i> | <code>(lazy <a href="#expression">Expression</a>)</code> |
| <i>open</i> | <code>(open <a href="#longcap">longCap</a> <a href="#expression">Expression</a><sup>*</sup>)</code> |
| <i>if</i> | <code>(if <a href="#expression">Expression</a> <a href="#expression">Expression</a> <a href="#expression">Expression</a><sup>?</sup>)</code> |
| <i>module_pack</i> | <code>(module <a href="#moduleexpr">ModuleExpr</a>)</code> |
| <i>module</i> | <code>(module <a href="#capident">capIdent</a> <a href="#moduleexpr">ModuleExpr</a> <a href="#expression">Expression</a><sup>*</sup>)</code> |
| <i>arrow</i> | <code>(=&gt; <a href="#fnargs">FnArgs</a> <a href="#expression">Expression</a><sup>*</sup>)</code> |
| <i>threading_last</i> | <code>(-&gt;&gt; <a href="#expression">Expression</a> <a href="#threaditem">ThreadItem</a><sup>*</sup>)</code> |
| <i>threading</i> | <code>(-&gt; <a href="#expression">Expression</a> <a href="#threaditem">ThreadItem</a><sup>*</sup>)</code> |
| <i>threading_as</i> | <code>(as-&gt; <a href="#expression">Expression</a> <a href="#pattern">Pattern</a> <a href="#expression">Expression</a><sup>*</sup>)</code> |
| <i>switch</i> | <code>(switch <a href="#expression">Expression</a> <a href="#pattern">Pattern</a> ⦅when <a href="#expression">Expression</a>⦆<sup>?</sup> <a href="#expression">Expression</a><sup>+</sup>)</code> |
| <i>switch_function</i> | <code>(switch _ <a href="#pattern">Pattern</a> ⦅when <a href="#expression">Expression</a>⦆<sup>?</sup> <a href="#expression">Expression</a><sup>+</sup>)</code> |
| <i>try</i> | <code>(try <a href="#expression">Expression</a> <a href="#pattern">Pattern</a> ⦅when <a href="#expression">Expression</a>⦆<sup>?</sup> <a href="#expression">Expression</a><sup>+</sup>)</code> |
| <i>array_index</i> | <code>([<a href="#expression">Expression</a>] <a href="#expression">Expression</a>)</code> |
| <i>js_object_attribute</i> | <code>(<a href="#string">string</a> <a href="#expression">Expression</a>)</code> |
| <i>setField</i> | <code>(&lt;- :<a href="#longident">longIdent</a> <a href="#expression">Expression</a> <a href="#expression">Expression</a>)</code> |
| <i>record_attribute</i> | <code>(:<a href="#longident">longIdent</a> <a href="#expression">Expression</a>)</code> |
| <i>fn_call</i> | <code>(<a href="#expression">Expression</a> <a href="#fncallarg">FnCallArg</a><sup>*</sup>)</code> |
| <i>constraint</i> | <code>(: <a href="#expression">Expression</a> <a href="#coretype">CoreType</a>)</code> |

### Pattern

| Name | Syntax |
| --- | --- |
| <i>ident</i> | <code><a href="#lowerident">lowerIdent</a></code> |
| <i>interval</i> | <code><a href="#constant">constant</a>..<a href="#constant">constant</a></code> |
| <i>constant</i> | <code><a href="#constant">constant</a></code> |
| <i>unit</i> | <code>()</code> |
| <i>ignored</i> | <code>_</code> |
| <i>array</i> | <code>[<a href="#pattern">Pattern</a><sup>*</sup> ⦅...<a href="#pattern">Pattern</a>⦆<sup>?</sup>]</code> |
| <i>tuple</i> | <code>(, <a href="#pattern">Pattern</a> <a href="#pattern">Pattern</a><sup>+</sup>)</code> |
| <i>empty_constr</i> | <code><a href="#longcap">longCap</a></code> |
| <i>poly</i> | <code>(`<a href="#capident">capIdent</a> <a href="#pattern">Pattern</a><sup>+</sup>)</code> |
| <i>empty_poly</i> | <code>`<a href="#capident">capIdent</a></code> |
| <i>exception</i> | <code>(exception <a href="#pattern">Pattern</a>)</code> |
| <i>constructor</i> | <code>(<a href="#longcap">longCap</a> <a href="#pattern">Pattern</a><sup>+</sup>)</code> |
| <i>object</i> | <code>{<a href="#patternobjectitem">PatternObjectItem</a><sup>+</sup>}</code> |
| <i>or</i> | <code>(\| <a href="#pattern">Pattern</a><sup>+</sup> )</code> |

### ModuleExpr

| Name | Syntax |
| --- | --- |
| <i>structure</i> | <code>(str <a href="#structure">Structure</a><sup>*</sup>)</code> |
| <i>ident</i> | <code><a href="#longcap">longCap</a></code> |

### TypeName

| Name | Syntax |
| --- | --- |
| <i>vbl</i> | <code>(<a href="#lowerident">lowerIdent</a> <a href="#typevariable">typeVariable</a><sup>+</sup>)</code> |
| <i>plain</i> | <code><a href="#lowerident">lowerIdent</a></code> |

### TypeKind

| Name | Syntax |
| --- | --- |
| <i>record</i> | <code>{<a href="#typeobjectitem">TypeObjectItem</a><sup>+</sup>}</code> |
| <i>constructors</i> | <code><a href="#typeconstructor">TypeConstructor</a><sup>+</sup></code> |
| <i>alias</i> | <code><a href="#coretype">CoreType</a></code> |

### TypeObjectItem

| Name | Syntax |
| --- | --- |
| <i>normal</i> | <code>:<a href="#lowerident">lowerIdent</a> <a href="#coretype">CoreType</a></code> |
| <i>punned</i> | <code>:<a href="#lowerident">lowerIdent</a></code> |

### TypeConstructor

| Name | Syntax |
| --- | --- |
| <i>no_args</i> | <code><a href="#capident">capIdent</a></code> |
| <i>args</i> | <code>(<a href="#capident">capIdent</a> <a href="#coretype">CoreType</a><sup>+</sup>)</code> |

### CoreType

| Name | Syntax |
| --- | --- |
| <i>constr_no_args</i> | <code><a href="#longident">longIdent</a></code> |
| <i>variable</i> | <code><a href="#typevariable">typeVariable</a></code> |
| <i>constructor</i> | <code>(<a href="#longident">longIdent</a> <a href="#coretype">CoreType</a><sup>+</sup>)</code> |
| <i>arrow</i> | <code>(=&gt; [<a href="#coretype">CoreType</a><sup>+</sup>] <a href="#coretype">CoreType</a>)</code> |

### typeVariable

 A type variable 

<code>'<a href="#lowerident">lowerIdent</a></code>

### FnCallArg

| Name | Syntax |
| --- | --- |
| <i>labeled</i> | <code>~<a href="#lowerident">lowerIdent</a>=<a href="#expression">Expression</a></code> |
| <i>punned</i> | <code>~<a href="#lowerident">lowerIdent</a></code> |
| <i>expr</i> | <code><a href="#expression">Expression</a></code> |

### ThreadItem

| Name | Syntax |
| --- | --- |
| <i>attribute</i> | <code>:<a href="#longident">longIdent</a></code> |
| <i>ident</i> | <code><a href="#longident">longIdent</a></code> |
| <i>emptyconstr</i> | <code><a href="#longcap">longCap</a></code> |
| <i>constructor</i> | <code>(<a href="#longcap">longCap</a> <a href="#expression">Expression</a><sup>+</sup>)</code> |
| <i>fn_call</i> | <code>(<a href="#expression">Expression</a> <a href="#fncallarg">FnCallArg</a><sup>+</sup>)</code> |

### ObjectItem

| Name | Syntax |
| --- | --- |
| <i>normal</i> | <code>:<a href="#longident">longIdent</a> <a href="#expression">Expression</a></code> |
| <i>punned</i> | <code>:<a href="#longident">longIdent</a></code> |

### FnArgs

| Name | Syntax |
| --- | --- |
| <i>single</i> | <code><a href="#lowerident">lowerIdent</a></code> |
| <i>unit</i> | <code>()</code> |
| <i>ignored</i> | <code>_</code> |
| <i>multiple</i> | <code>[<a href="#fnarg">FnArg</a><sup>+</sup>]</code> |

### FnArg

| Name | Syntax |
| --- | --- |
| <i>destructured</i> | <code>~<a href="#lowerident">lowerIdent</a>⦅:<a href="#coretype">CoreType</a>⦆<sup>?</sup> as <a href="#pattern">Pattern</a></code> |
| <i>optional</i> | <code>~<a href="#lowerident">lowerIdent</a>=?</code> |
| <i>defaulted</i> | <code>~<a href="#lowerident">lowerIdent</a>⦅:<a href="#coretype">CoreType</a>⦆<sup>?</sup>=<a href="#expression">Expression</a></code> |
| <i>labeled</i> | <code>~<a href="#lowerident">lowerIdent</a>⦅:<a href="#coretype">CoreType</a>⦆<sup>?</sup></code> |
| <i>unlabeled</i> | <code><a href="#pattern">Pattern</a></code> |

### PatternObjectItem

| Name | Syntax |
| --- | --- |
| <i>normal</i> | <code>:<a href="#longident">longIdent</a> <a href="#pattern">Pattern</a></code> |
| <i>punned</i> | <code>:<a href="#longident">longIdent</a></code> |

### longIdent

 A potentially-namespaced lower-case identifier 

<code>⦅<a href="#longcap_">longCap_</a>.⦆<sup>?</sup><a href="#lowerident">lowerIdent</a></code>

### longCap

 A potentially-namespaced capital identifier 

<code><a href="#longcap_">longCap_</a></code>

### longCap_

| Name | Syntax |
| --- | --- |
| <i>dot</i> | <code><a href="#longcap_">longCap_</a>.<a href="#capident">capIdent</a></code> |
| <i>lident</i> | <code><a href="#capident">capIdent</a></code> |

### constant

| Name | Syntax |
| --- | --- |
| <i>float</i> | <code><a href="#float">float</a></code> |
| <i>int</i> | <code><a href="#int64">int64</a></code> |
| <i>string</i> | <code><a href="#string">string</a></code> |
| <i>char</i> | <code><a href="#char">char</a></code> |

### capIdent

 A simple identifier starting with a capital letter 

<code>A…Z<a href="#identchar">identchar</a><sup>*</sup></code>

### lowerIdent

 A simple identifier starting with a lower-case letter 

<code>a…z<a href="#identchar">identchar</a><sup>*</sup></code>

### identchar

| Name | Syntax |
| --- | --- |
| <i></i> | <code><a href="#alpha">alpha</a></code> |
| <i></i> | <code>0…9</code> |
| <i></i> | <code>_</code> |

### int64

 An int constant 

<code>0…9<sup>+</sup></code>

### float

 A float constant 

<code>0…9<sup>+</sup>.0…9<sup>+</sup></code>

### string

 A string constant 

<code>"<a href="#strchar">strchar</a><sup>*</sup>"</code>

### strchar

| Name | Syntax |
| --- | --- |
| <i></i> | <code>\\<i>any</i></code> |
| <i></i> | <code><i>any</i></code> |

### char

 A char constant 

<code>'<a href="#charchar">charchar</a>'</code>

### charchar

| Name | Syntax |
| --- | --- |
| <i></i> | <code>\\<i>any</i></code> |
| <i></i> | <code><i>any</i></code> |

### reserved

| Name | Syntax |
| --- | --- |
| <i></i> | <code>fun</code> |
| <i></i> | <code>let</code> |
| <i></i> | <code>and</code> |
| <i></i> | <code>as</code> |
| <i></i> | <code>type</code> |
| <i></i> | <code>switch</code> |
| <i></i> | <code>exception</code> |
| <i></i> | <code>external</code> |
| <i></i> | <code>of</code> |
| <i></i> | <code>module</code> |
| <i></i> | <code>rec</code> |
| <i></i> | <code>open</code> |
| <i></i> | <code>import</code> |
| <i></i> | <code>try</code> |
| <i></i> | <code>catch</code> |
| <i></i> | <code>from</code> |

### alpha

| Name | Syntax |
| --- | --- |
| <i></i> | <code>a…z</code> |
| <i></i> | <code>A…Z</code> |

### operator

 An operator 

<code><a href="#opchar">opChar</a><sup>+</sup></code>

### reservedOps

| Name | Syntax |
| --- | --- |
| <i></i> | <code>=&gt;</code> |
| <i></i> | <code>-&gt;</code> |
| <i></i> | <code>-&gt;&gt;</code> |
| <i></i> | <code>...</code> |

### opChar

| Name | Syntax |
| --- | --- |
| <i></i> | <code>!</code> |
| <i></i> | <code>$</code> |
| <i></i> | <code>%</code> |
| <i></i> | <code>&</code> |
| <i></i> | <code>*</code> |
| <i></i> | <code>+</code> |
| <i></i> | <code>-</code> |
| <i></i> | <code>.</code> |
| <i></i> | <code>/</code> |
| <i></i> | <code>&lt;</code> |
| <i></i> | <code>=</code> |
| <i></i> | <code>&gt;</code> |
| <i></i> | <code>?</code> |
| <i></i> | <code>@</code> |
| <i></i> | <code>^</code> |
| <i></i> | <code>\|</code> |
| <i></i> | <code>~</code> |


