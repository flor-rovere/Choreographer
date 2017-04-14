module DataType where

data Movement = Up 
              | Down 
              | Side 
              | BRight 
              | BLeft 
              | BCenter 

data BodyPart = RightArm 
              | LeftArm 
              | RightLeg 
              | LeftLeg 
              | StartPace 
              | EndPace 
              | Head 
  deriving Eq

data States = Step BodyPart Movement 
            | StartMany Int BodyPart Movement 
            | EndMany Int BodyPart Movement
            | End

-- Step será una línea de comandos ingresados (un paso)
type Step = [States]

--ListSt serán todos los comandos ingresados (la coreografía)
type ListSt = [Step]

data ActualPosition = Pos
    { rightArm :: Movement
    , leftArm  :: Movement
    , rightLeg :: Movement
    , leftLeg  :: Movement
    , pace     :: Movement
    , bhead    :: Movement
    }

instance Show (ActualPosition) where
    show = show

initPosition :: ActualPosition
initPosition = Pos {rightArm=Down, leftArm=Down, rightLeg=Down,
                    leftLeg=Down, pace=BCenter, bhead=BCenter}

data Error = Ok | Not_Valid | Repeated
