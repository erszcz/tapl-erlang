#mod_use "support.ml";;
#mod_use "syntax.ml";;
#mod_use "parser.ml";;
#mod_use "lexer.ml";;
#mod_use "core.ml";;
#mod_use "main.ml";;

#trace Core.tyeqv;;
#trace Core.simplifyty;;
#trace Core.computety;;

1 ;;

Main.process_file "test.f" Syntax.emptycontext;;
