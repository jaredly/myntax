const input = process.stdin.read()
process.stdout.write(input.replace(/[\@[^\]]*\]//g, ''))