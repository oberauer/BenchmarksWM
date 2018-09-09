# Paul Bays' wrap function -> signed angular difference (radians)!
wrap = function(angle) {
  wangle <- ( (angle + pi) %% (2*pi) ) - pi
  return(wangle)
}
