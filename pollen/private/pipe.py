# This allows us to launch Python and pygments once, and pipe to it
# continuously. Input format is:
#
#     <lexer-name>
#     <code>
#     ...
#     __END__
#
#  OR
#
#     __EXIT__
#
# Output format is:
#
#     <html>
#     ...
#     __END__

import sys
import optparse
from pygments import highlight
from pygments.lexers import get_lexer_by_name
from pygments.util import ClassNotFound
from pygments.formatters import HtmlFormatter

parser = optparse.OptionParser()
parser.add_option('--linenos', action="store_true", dest="linenos")
parser.add_option('--cssclass', default="source", dest="cssclass")
(options, _) = parser.parse_args()

formatter = HtmlFormatter(linenos=options.linenos,
                          cssclass=options.cssclass,
                          encoding="utf-8")

lexer = ""
code = ""
py_version = sys.version_info.major
sys.stdout.write("ready\n")
sys.stdout.flush()
while 1:
    line_raw = sys.stdin.readline()
    if not line_raw:
        break
    # Without trailing space, \n, or \n
    line = line_raw.rstrip()
    if line == '__EXIT__':
        break
    elif line == '__END__':
        # Lex input finished. Lex it.
        if py_version >= 3:
          sys.stdout.write(highlight(code, lexer, formatter).decode("utf-8"))
        else:
          sys.stdout.write(highlight(code, lexer, formatter))
        sys.stdout.write('\n__END__\n')
        sys.stdout.flush()
        lexer = ""
        code = ""
    elif lexer == "":
        # Starting another lex. First line is the lexer name.
        try:
            lexer = get_lexer_by_name(line, encoding="guess")
        except ClassNotFound:
            lexer = get_lexer_by_name("text", encoding="guess")
    else:
        # Accumulate more code
        # Use `line_raw`: Do want trailing space, \n, \r
        code += line_raw

exit(0)
