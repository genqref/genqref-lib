(ns genqref-lib.core
  (:use [potemkin])
  (:require [genqref-lib.api :as api]
            [genqref-lib.helper :as helper]
            [genqref-lib.util :as util]
            ))

(import-vars

 [genqref-lib.util

  sym]

 [genqref-lib.api

  register!
  init!
  state

  xy
  role
  ship-has?
  get-waypoint
  min-sec
  remaining-capacity
  remaining-units
  units-in-cargo
  markets-selling
  lookup-and-merge-coordinates
  best-market-for
  system-where-ship
  ship-by-name
  waypoints-with-traits
  waypoints-of-type
  ships-with-role
  waypoints-of-system
  jumpgates-of-system
  active-contracts
  active-contract
  closest-market-to-waypoint
  jumpgate-where-ship!

  register-hooks
  reset-hooks!

  refresh!
  query!

  purchase-ship!
  orbit!
  refine!
  chart!
  dock!
  survey!
  extract!
  jettison!
  jump!
  navigate!
  flight-mode!
  warp!
  sell-cargo!
  scan-systems!
  scan-waypoints!
  scan-ships!
  refuel!
  purchase-cargo!
  transfer!
  negotiate-contract!
  deliver-contract!
  accept-contract!
  fulfill-contract!
  install-mount!
  remove-mount!

  sell-all-cargo!
  jettison-all-cargo!]

 [genqref-lib.helper

  markets-in-system])
