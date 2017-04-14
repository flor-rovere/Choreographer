module Body where

import System.Exit
import Graphics.Proc

import States
import DataType

--setup inicializa el cuadro en el que se va a dibujar
setup :: ListSt -> Float -> (Float, Float) -> Pio ((Float, Float), (Float, Float), ActualPosition, ListSt)
setup xs frame ps = do
    size (fst ps, snd ps)
    frameRate frame
    let zs     = DataType.initPosition
        center = ((fst ps)* 0.5,(snd ps)*0.5)
     in return (center, center, zs, xs)

--draw dibuja un paso, que puede constar de un sólo movimiento o de varios
draw :: ((Float, Float), (Float, Float), ActualPosition, ListSt) -> Pio ()
draw (_,_,_,[]) = liftIO (exitWith ExitSuccess)
draw (center, start_point, zs, xs) = do
    background (grey 230)
    local $ do
        drawbody start_point 
        drawleftarm (leftArm zs)
        drawrightarm (rightArm zs)
        drawhead (bhead zs)
        drawleftleg (leftLeg zs)
        drawrightleg (rightLeg zs)

drawbody start_point = do
        translate start_point
        fill (grey 68)
        rect (-30,-30) (30,50)

drawleftarm x = case x of
    Side -> do
        translate (-30,-30)
        line (0,0) (-30,0)
        line (-30,0) (-60,0)
    Up   -> do
        translate (-30,-30)
        line (0,0) (-5,-30)
        line (-5,-30) (0,-60)
    Down -> do
        translate (-30,-30)
        line (0,0) (-5,30)
        line (-5,30) (0,60)
    _    ->liftIO (die "Error: Se ingresó un movimiento no válido")

drawrightarm x = case x of
    Side -> do
        translate (30,0)
        line (0,0) (30,0)
        line (30,0) (60,0)
    Up   -> do
        translate (30,0)
        line (0,0) (5,-30)
        line (5,-30) (0,-60)
    Down -> do
        translate (30,0)
        line (0,0) (5,30)
        line (5,30) (0,60)
    _    -> liftIO (die "Error: Se ingresó un movimiento no válido")

drawhead x = case x of
    BCenter -> do
        translate (-15,-13)
        drawheadaux
        translate (0,-7)
    BLeft   -> do
        translate (-18,-13)
        rotate 0.9
        drawheadaux
        translate (2,1)
        translate (0,-7)
        rotate 0.1
    BRight  -> do
        translate (-12,-13)
        rotate 0.1
        drawheadaux
        translate (-3,1)
        translate (0,-7)
        rotate 0.9
    _       -> liftIO (die "Error: Se ingresó un movimiento no válido")

drawheadaux = do
    fill (grey 165)
    circle 13 (0,0)
    translate (-4,-3)
    line (0,0) (0,-5)
    translate (8,0)
    line (0,0) (0,-5)
    translate (-4,3)
    fill (grey 125)
    circle 1 (0,0)
    translate (0,-12)
    line (0,0) (0,-7)
    line (0,0) (-5,-3)
    line (0,0) (6,-4)
    translate (0,19)
    line (5,-2) (0,0)
    line (0,0) (-5,-2)

drawleftleg x = case x of
    Side -> do
        translate (-10,62)
        rotate 0.1
        fill (grey 125)
        rect (0,0) (5,45)
        rotate 0.9
        translate (0,1)
    Down -> do
        translate (-10,63)
        fill (grey 125)
        rect (0,0) (5,40)
    _    -> liftIO (die "Error: Se ingresó un movimiento no válido")

drawrightleg x = case x of
    Side -> do
        translate (15,2)
        rotate 0.9
        fill (grey 125)
        rect (0,0) (5,45)
        translate (0,-2)
    Down -> do
        translate (15,0)
        fill (grey 125)
        rect (0,0) (5,40)
    _    -> liftIO (die "Error: Se ingresó un movimiento no válido")

--update actualiza los parámetros para dibujar el próximo paso
update :: ((Float, Float), (Float, Float), ActualPosition, ListSt) -> Pio ((Float, Float), (Float, Float), ActualPosition, ListSt)
update (center, start_point, zs, (x:xs)) = case x of
    (End:_)                -> return (center,start_point,zs,[])
    (Step StartPace i:ys)  -> updatePace center start_point zs (x:xs)
    (Step EndPace i:ys)    -> updatePace center start_point zs (x:xs)
    ((StartMany n v i):ys) -> updateMany center start_point zs (x:xs)
    ((EndMany n v i):ys)   -> updateMany center start_point zs (x:xs)
    _                      -> return (center, start_point, snd (runState (States.update x) zs), xs)

updateMany :: (Float, Float) -> (Float, Float) -> ActualPosition -> ListSt -> Pio ((Float, Float), (Float, Float), ActualPosition, ListSt)
updateMany center start_point zs (st@((StartMany n v i):ys):xs) = case n of
    0 -> return (center, start_point, zs, ys:xs)
    1 -> case v of
        StartPace -> updateManyPace center start_point zs (st:xs)
        EndPace   -> updateManyPace center start_point zs (st:xs)
        _         -> let zs' = snd (runState (States.update [Step v i]) zs)
                      in return (center, start_point, zs', ((EndMany n v i):ys):xs)
    _ -> case v of
        StartPace -> updateManyPace center start_point zs (st:xs)
        EndPace   -> updateManyPace center start_point zs (st:xs)
        _         -> let zs' = snd (runState (States.update [Step v i]) zs)
                      in return (center, start_point, zs', ((EndMany n v i):ys):xs)
updateMany center start_point zs (st@((EndMany n v i):ys):xs) = case v of
    Head      -> let zs' = snd (runState (States.update [Step Head BCenter]) zs)
                  in return (center, start_point, zs',((StartMany (n-1) v i):ys):xs)
    StartPace -> updateManyPace center start_point zs (st:xs)
    EndPace   -> updateManyPace center start_point zs (st:xs)
    _         -> let zs' = snd (runState (States.update [Step v Down]) zs)
                  in return (center, start_point, zs',((StartMany (n-1) v i):ys):xs)

updatePace :: (Float, Float) -> (Float, Float) -> ActualPosition -> ListSt -> Pio ((Float, Float), (Float, Float), ActualPosition, ListSt)
updatePace center start_point zs (((Step StartPace x):ys):xs) = case x of
    BRight -> let zs' = snd (runState (States.update [Step RightLeg Side]) zs)
               in return (center, start_point, zs', ((Step EndPace BRight):ys):xs)
    BLeft  -> let zs' = snd (runState (States.update [Step LeftLeg Side]) zs)
               in return (center, start_point, zs', ((Step EndPace BLeft):ys):xs)
    _      -> liftIO (die "Error: Se ingresó un movimiento no válido")
updatePace center start_point zs (((Step EndPace x):ys):xs) = case x of
    BRight -> let dim = if start_point > ((fst(center))*2, snd(center)) 
                         then center 
                         else start_point + (30,0)
                  zs' = snd (runState (States.update (Step RightLeg Down:Step EndPace BRight:ys)) zs)
               in return (center, dim, zs', xs)
    BLeft  -> let dim = if start_point < (0,snd(center)) 
                         then center 
                         else start_point - (30,0)
                  zs' = snd (runState (States.update (Step LeftLeg Down:Step EndPace BLeft:ys)) zs)
               in return (center, dim, zs', xs)
    _      -> liftIO (die "Error: Se ingresó un movimiento no válido")

updateManyPace :: (Float, Float) -> (Float, Float) -> ActualPosition -> ListSt -> Pio ((Float, Float), (Float, Float), ActualPosition, ListSt)
updateManyPace center start_point zs (((StartMany n v i):ys):xs) = do
    (center', start_point', zs', ((Step v' i'):ys'):xs') <- updatePace center start_point zs (((Step v i):ys):xs)
    return (center', start_point', zs', ((EndMany n v' i'):ys'):xs')
updateManyPace center start_point zs (((EndMany n v i):ys):xs) = do
    (center', start_point', zs', xs') <- updatePace center start_point zs (((Step v i):ys):xs)
    return (center', start_point', zs', [StartMany (n-1) StartPace i]:xs')
