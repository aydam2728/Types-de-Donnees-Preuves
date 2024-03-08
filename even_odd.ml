let rec even (n : int) : bool = (n = 0) || odd (n - 1) ;;
let rec odd (n : int) : bool = (n <> 0) && even (n - 1) ;;
even 7 ;;

