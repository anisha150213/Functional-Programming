import Prelude hiding (lookup)

data Exp = Num Int 
 | Plus Exp Exp 
 | Minus Exp Exp
 | Times Exp Exp
 | Div Exp Exp
 | Var String
 | Let String Exp Exp
 | App Exp Exp
 | Fn String Exp 
  deriving (Show)

type Ctx = [(String, Val)]

data Val = CVal Int
 | FnVal String Exp Ctx
 | Error String 
  deriving (Show) 

-- implement
lookup :: Ctx -> String -> Val
lookup [] y = Error ("Variable " ++ y ++" is not found")
lookup ((x,v ): ctx)  y = if(x == y) then v else lookup ctx y



-- implement
eval :: Exp -> Ctx -> Val
eval exp ctx =  case exp of Num x -> CVal x
                            Plus e1 e2 -> case ((eval e1 ctx), (eval e2 ctx)) of (CVal v1 , CVal v2) -> CVal (v1 + v2)
                                                                                 (Error v1, _) ->  Error v1
                                                                                 (_, Error v2) -> Error v2
                                                                                 
                                                                                 (err, CVal v2) -> Error ("Plus error: " ++ show err ++ " is not a number" )
                                                                                 (CVal v1, err) -> Error ("Plus error: " ++ show err ++ " is not a number")
                                                                                 
                            Minus e1 e2 -> case ((eval e1 ctx), (eval e2 ctx)) of (CVal v1 , CVal v2) -> CVal (v1 - v2)
                                                                                  (Error v1, _) ->  Error v1
                                                                                  (_, Error v2) -> Error v2
                                                                                  (err, CVal v2) -> Error ("Minus error: " ++ show err ++ " is not a number")
                                                                                  (CVal v1, err) -> Error ("Minus error: " ++ show err ++ " is not a number")
                                                                                  
                            Times e1 e2 -> case ((eval e1 ctx), (eval e2 ctx)) of (CVal v1 , CVal v2) -> CVal (v1 * v2)
                                                                                  (Error v1, _) ->  Error v1
                                                                                  (_, Error v2) -> Error v2
                                                                                  (err, CVal v2) -> Error ("Times error: " ++ show err ++ " is not a number")
                                                                                  (CVal v1, err) -> Error ("Times error: " ++ show err ++ " is not a number")

                            Div e1 e2 -> case ((eval e1 ctx), (eval e2 ctx)) of (CVal v1 , CVal 0) -> Error ("Division by zero error : Div (Num " ++  show v1 ++ ") (Num 0)")
                                                                                (CVal v1 , CVal v2) -> CVal (div v1 v2)
                                                                                (Error v1, _) ->  Error v1
                                                                                (_, Error v2) -> Error v2
                                                                                (err,CVal v2) -> Error ("Div error: " ++ show err ++ " is not a number")
                                                                                (CVal v1, err) -> Error ("Div error: " ++ show err ++ " is not a number")
                            Var x -> lookup ctx x
                            Fn x e -> FnVal x e ctx 
                            Let x e1 e2 -> case (eval e1 ctx, e2) of (Error m, _) -> Error m
                                                                     (v1, e2) -> case (eval e2 ((x, v1):ctx)) of CVal v -> CVal v
                                                                                                                 Error m -> Error m 

                            App e1 e2 -> case (eval e1 ctx, eval e2 ctx) of (FnVal x e ctx1, v2) -> let contxt = (x, v2) : ctx1                                                                            
                                                                                                        helper = eval e contxt
                                                                                                        helper2 value = case value of CVal(v) -> CVal(v)
                                                                                                                                      Error(m) -> Error(m)
                                                                                                    in helper2 helper
                                                                            (CVal v, _) -> Error("Application error: CVal " ++ show v ++ " is not a function")                                                      
                                                                            (Error m, _) -> Error m

         

 
                                                                                                         
main = do 
          let t1 = Let "y" (Num 10) 
                    (Let "f" (Fn "x" (Plus (Var "x") (Var "y"))) 
                      (Let "y" (Num 20)
                        (App (Var "f") (Num 5))))
          let t2 = Let "y" (Num 10) 
                    (Let "f" (Fn "x" (Plus (Var "x") (Var "z"))) 
                      (Let "y" (Num 20)
                        (App (Var "f") (Num 5))))
          let t3 = Div (Num 10) (Num 0)
          let t4 = Plus (Num 10) (Minus (Num 20) (Fn "x" (Num 3)))
          let t5 = App (Num 10) (Num 20)
          let t6 = Let "f" (Num 10) (App (Var "f") (Num 20))
          let t7 = Let "x" (Plus (Num 10) (Fn "x" (Var "x"))) (Plus (Num 0) (Num 20))
          
          let t = [t1, t2, t3,t4, t5, t6, t7]
          sequence_ $ map (\x -> print $ eval x []) t 

          
