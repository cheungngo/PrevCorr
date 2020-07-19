# Function to calculate the multiplier for equivalent dosage (ref = fluoxetine)

drug2mult = function (x) {
  if (x == 1) {
    mult = 1
  } else {
    if (x == 2) {
      mult = 40/98.5
    } else {
      if (x == 3) {
        mult = 40/34.5
      } else {
        if (x == 4) {
          mult = 40/18
        } else {
          if (x == 5) {
            mult = 40/(18*(27.6/13.9))
          } else {
            if (x == 6) {
              mult = 40/143.3
            } else {
              mult = 0
            }}}}}}
  return(mult)
}


drug_class = function(x) {
  if (x == "Agomelatine") {
    return("MTASA")
  } else if (x == "Amitriptyline") {
    return("TCA")
  } else if (x == "Bupropion") {
    return("NDRI")
  } else if (x == "Clomipramine") {
    return("TCA")
  } else if (x == "Deanxit") {
    return("TCA")
  } else if (x == "Desvenlafaxine") {
    return("SNRI")
  } else if (x == "Dothiepin") {
    return("TCA")
  } else if (x == "Duloxetine") {
    return("SNRI")
  } else if (x == "Imipramine") {
    return("TCA")
  } else if (x == "Mianserin") {
    return("NASSA")
  } else if (x == "Milnacipran") {
    return("SNRI")
  } else if (x == "Mirtazapine") {
    return("NASSA")
  } else if (x == "Nortriptyline") {
    return("TCA")
  } else if (x == "Pregabalin") {
    return("GABA")
  } else if (x == "Trazodone") {
    return("NARI")
  } else if (x == "Trimipramine") {
    return("TCA")
  } else if (x == "Venlafaxine") {
    return("SNRI")
  } else if (x == "Vortioxetine") {
    return("SMS")
  } else {
    return(NA)
  }
}
