# This allows us to launch Python and pygments once, and pipe to it
# continuously.
#
# There are four options:
#   1. Language (used to determine the lexer)
#   2. Line numbers to highlight
#   3. Encoding to use for the output
#   4. HTML Class to use for the output
#
# These can be specified as arguments when this script is first invoked, or on a
# per invocation.
#
# To set the options for each invocation, the format is:
#
#     __LANG__ <lexer-name>
#     __LINENOS__ <true|false>
#     __LINES__ <list-of-highlighted-lines>
#     __CSS__ <CSS-class>
#     __ENC__ <encoding>
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

from __future__ import print_function
import sys
import optparse

from pygments import highlight
from pygments.lexers import get_lexer_by_name
from pygments.util import ClassNotFound
from pygments.formatters import HtmlFormatter

def get_lexer(lang):
    if not lang:
        get_lexer_by_name("text", encoding="guess")

    try:
        return get_lexer_by_name(lang, encoding="guess")
    except ClassNotFound:
        print("No lexer was found for the given language. Using plain text instead.", file=sys.stderr)
        return  get_lexer_by_name("text", encoding="guess")

parser = optparse.OptionParser()
parser.add_option('--linenos', action="store_true", dest="linenos")
parser.add_option('--cssclass', default="source", dest="cssclass")
parser.add_option('--encoding', default="utf-8", dest="encoding")
parser.add_option('--language', dest="language")
(options, _) = parser.parse_args()

# Set initial options
config = {
    'linenos': options.linenos,
    'cssclass': options.cssclass,
    'encoding': options.encoding,
    'hl_lines': []
}
lexer = get_lexer(options.language)
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
        formatter = HtmlFormatter(linenos=config['linenos'],
                                  cssclass=config['cssclass'],
                                  encoding=config['encoding'],
                                  hl_lines=config['hl_lines'])
        if py_version >= 3:
          sys.stdout.write(highlight(code, lexer, formatter).decode("utf-8"))
        else:
          sys.stdout.write(highlight(code, lexer, formatter))
        sys.stdout.write('\n__END__\n')
        sys.stdout.flush()

        # Reset the configuration for the next invocation. Most options are
        # actually persisted between runs, except for the code itself and the
        # lines to be highlighted.
        code = ""
        config['hl_lines'] = []

    elif code == "":
        # Only check for new options at the beginning of a a fresh invocation
        if line.startswith("__LANG__"):
        # Use the provided language to find the appropriate lexer.
            try:
                lang = line.split()[1]
                lexer = get_lexer(lang)
            except IndexError:
                print("No lexer was found for the given language. Using plain text instead.", file=sys.stderr)
                lexer = get_lexer_by_name("text", encoding="guess")

        elif line.startswith("__LINENOS__"):
            try:
                option = line.split()[1]
                if option.lower() == "true":
                    config['linenos'] = True
                elif option.lower() == "false":
                    config['linenos'] = False
                else:
                    pass
            except IndexError:
                print("__LINENOS__ option must be given a `true` or `false` value",
                      file=sys.stderr)

        elif line.startswith("__LINES__"):
            # The list of lines to highlight is formatted as string of
            # whitespace-separated integers
            lines = line.split()[1:]
            config['hl_lines'] = [int(str) for str in lines]

        elif line.startswith("__CSS__"):
            try:
                config['cssclass'] = line.split[1]
            except IndexError:
                print("Could not parse CSS class line.", file=sys.stderr)

        elif line.startswith("__ENC__"):
            try:
                config['encoding'] = line.split[1]
            except IndexError:
                print("Could not parse encoding line.", file=sys.stderr)
        else:
            # Done with configuration for this invocation, start accumulating
            # code. Use `line_raw` because we want trailing space, \n, \r
            code += line_raw

    else:
        # Accumulate more code
        # Use `line_raw`: Do want trailing space, \n, \r
        code += line_raw

exit(0)
