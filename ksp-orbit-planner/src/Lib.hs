{-# LANGUAGE OverloadedStrings #-}


-- ##http://www.braeunig.us/space/orbmech.htm
-- ## http://www.braeunig.us/space/interpl.htm
-- import Data.Text as T

module Lib
     where

someFunc :: IO ()
someFunc = putStrLn "test"

data Measure a = Measure Double deriving (Show)

--instance (Functor Measure)  where
  --fmap f (Measure a) = Measure $ f a

data Meter
data Meter2
data Meter3
data Second
data Second2
data Minute
data Hour
data GravConst
data Velocity
data AngVelocity -- M/S
data Joules
data DeltaV -- change in velocity

liftMeasure :: Measure Double -> Double
liftMeasure (Measure x) = x

toDeltaV :: Measure Velocity -> Measure Velocity -> Measure DeltaV
toDeltaV (Measure v1) (Measure v2) = Measure $ (-) v1 v2

-- move to fmap
addMeasure :: Measure a -> Measure a -> Measure a
addMeasure (Measure x1) (Measure y1) = Measure $ x1 + y1

f2Measure :: (Double -> Double -> Double ) -> Measure a -> Measure a -> Measure a
f2Measure f (Measure x1) (Measure x2) = Measure $   f x1 x2

f3Measure :: (Double -> Double -> Double -> Double) -> Measure a -> Measure a -> Measure a -> Measure a
f3Measure f (Measure x1) (Measure x2) (Measure x3) = Measure $ f x1 x2 x3

toVelocity :: Double -> Measure Velocity
toVelocity v1 = Measure $ v1

toAngVelocity :: Double -> Measure AngVelocity
toAngVelocity av1 = Measure $ av1

toGraveConst :: Double -> Measure GravConst
toGraveConst g = Measure g

toSeconds :: Double -> Measure Second
toSeconds s1 = Measure s1

toMeter :: Double -> Measure Meter
toMeter x = Measure x

toMeter2 :: Double -> Measure Meter2
toMeter2 x = Measure x

toMeter3 :: Double -> Measure Meter3
toMeter3 x = Measure x

sqaureMeter :: Measure Meter -> Measure Meter -> Measure Meter2
sqaureMeter (Measure m1) (Measure m2) = Measure (m1 * m2)

fromMeter2 :: Measure Meter2 -> (Measure Meter, Measure Meter) 
fromMeter2 (Measure m1) = (Measure m1, Measure 1)

cubeMeter :: Measure Meter -> Measure Meter -> Measure Meter -> Measure Meter3
cubeMeter (Measure m1) (Measure m2) (Measure m3) = Measure (m1 * m2 * m3)


--- functions from orbit.R
aveAngVelocity :: Measure GravConst -> Measure Meter -> Measure AngVelocity
aveAngVelocity (Measure g1) (Measure m1) = Measure $ sqrt $ (m1 ^ 3) / g1

periodFromAngVelocity :: Measure AngVelocity -> Measure Second
periodFromAngVelocity (Measure av1) = Measure $ av1 / ( 2 * pi)

angVelocityFromPeriod :: Measure Second -> Measure AngVelocity
angVelocityFromPeriod (Measure s1) = Measure $ (2 * pi)/ s1

periodFromSemiMajorAxis :: Measure GravConst -> Measure Meter -> Measure Second
periodFromSemiMajorAxis (Measure g1) (Measure m1) = Measure $ sqrt $ (4 * (pi ^ 2) * (m1 ^ 3)) / g1

aveOrbitalVelocity :: Measure GravConst -> Measure Meter -> Measure Velocity
aveOrbitalVelocity (Measure g1) (Measure m1) = 
  let period = sqrt((4 * (pi ^ 2) * (g1 ^ 3))/ g1)
      circ = 2 * pi * m1
  in Measure $ circ / period

orbitalPeriod :: Measure GravConst -> Measure Meter -> Measure Second
orbitalPeriod (Measure g1) (Measure m1) = Measure $ sqrt (4 * (pi ^ 2) * (m1 ^ 3)) / g1

orbitalVelocityCirc :: Measure GravConst -> Measure Meter -> Measure Velocity
orbitalVelocityCirc (Measure g1) (Measure m1)  =  Measure $ sqrt $ (/) g1 m1

velocityFromSemiAxis :: Measure Meter -> Measure Meter -> Measure GravConst -> Measure Velocity
velocityFromSemiAxis (Measure a1) (Measure r1) (Measure g1) = 
  Measure $ sqrt $ g1 * (r1' - a1')
  where 
    a1' = 1 / a1
    r1' = 2 / r1

hoffTransDeltaV :: Measure GravConst -> Measure Meter -> Measure Meter -> Measure DeltaV
hoffTransDeltaV g@(Measure g1) radiusA@(Measure rA) radiusB@(Measure rB) =
  let transAxis = toMeter $ (rA + rB) / 2
      velAinit = orbitalVelocityCirc g radiusA
      velBfinal = aveOrbitalVelocity g radiusB
      velAtx = velocityFromSemiAxis transAxis radiusA g
      velBtx = velocityFromSemiAxis transAxis radiusB g
      dvLeave = toDeltaV velAtx velAinit
      dvEnter = toDeltaV velBfinal velBtx
  in 
    f2Measure (+) dvLeave dvEnter
{-
hoff_trans_stats <- function(rA, rB, GM,mass=1){
    trans_axis = (rA + rB)/2
    v_iA = orbital_velocity_circ(GM,rA) # initial A
    v_fB = orbital_velocity_ave(GM,rB)  # final B
    v_txa = vel_from_semi_axis_radius(major_axis=trans_axis,radius=rA,GM=GM)
    v_txb = vel_from_semi_axis_radius(major_axis=trans_axis,radius=rB,GM=GM)
    list( init_delta_v = v_txa - v_iA,
          final_delta_v = v_fB - v_txb,
          total_delta_v = (v_txa - v_iA) + (v_fB - v_txb),
          energy_A = energy_from_vel_rad_mass(v_iA,rA,m=1,GM=Kerbin_GM),
          energy_txa = energy_from_vel_rad_mass(v_txa,rA,m=1,GM=Kerbin_GM),
          energy_txb = energy_from_vel_rad_mass(v_txb,rB,m=1,GM=Kerbin_GM),
          energy_fB = energy_from_vel_rad_mass(v_fB,rB,m=1,GM=Kerbin_GM))

}
-}
