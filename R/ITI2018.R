"ITI2018" <- Vectorize(function(income, category=1) {
  if (income < 0) stop("Error: Income must be > 0.")
  if (category == 1){
    if (income <= 300000)
    {return(0)}
    if (income > 300000 && income <= 500000) {
      TI <- income - 300000
      IT <- TI*0.05
      EC <- IT*0.03
      return(IT+EC)}
    if (income > 500000 && income <= 1000000) {
      TI <- income - 500000
      IT <- (TI*0.20) + 12500
      EC <- IT*0.03
      return(IT+EC)}
    if (income > 1000000 && income <= 5000000) {
      TI <- income - 1000000
      IT <- (TI*0.30) + 112500
      EC <- IT*0.03
      return(IT+EC)}
    if (income > 5000000 && income <= 10000000) {
      TI <- income - 1000000
      IT <- (TI*0.30) + 112500
      SC <- IT*0.10
      MR <- if((income-5000000) < SC) (SC-((income-5000000)-(income-5000000)*.30))else(0)
      NSC <- SC-MR
      EC <- (IT+NSC)*0.03
      return(IT+NSC+EC)}
    if (income > 10000000) {
      TI <- income - 1000000
      IT <- (TI*0.30) + 112500
      SC <- IT*0.15
      MR <- if((income-10000000) < SC) (SC-((income-10000000)-(income-10000000)*.30))else(0)
      NSC <- SC-MR
      EC <- (IT+NSC)*0.03
      return(IT+NSC+EC)}
  }
  if (category == 2){
    if (income <= 350000)
    {return(0)}
    if (income > 350000 && income <= 500000) {
      TI <- income - 300000
      IT <- TI*0.05
      EC <- IT*0.03
      return(IT+EC)}
    if (income > 500000 && income <= 1000000) {
      TI <- income - 500000
      IT <- (TI*0.20) + 10000
      EC <- IT*0.03
      return(IT+EC)}
    if (income > 1000000 && income <= 5000000) {
      TI <- income - 1000000
      IT <- (TI*0.30) + 112500
      EC <- IT*0.03
      return(IT+EC)}
    if (income > 5000000 && income <= 10000000) {
      TI <- income - 1000000
      IT <- (TI*0.30) + 110000
      SC <- IT*0.10
      MR <- if((income-5000000) < SC) (SC-((income-5000000)-(income-5000000)*.30))else(0)
      NSC <- SC-MR
      EC <- (IT+NSC)*0.03
      return(IT+NSC+EC)}
    if (income > 10000000) {
      TI <- income - 1000000
      IT <- (TI*0.30) + 110000
      SC <- IT*0.15
      MR <- if((income-10000000) < SC) (SC-((income-10000000)-(income-10000000)*.30))else(0)
      NSC <- SC-MR
      EC <- (IT+NSC)*0.03
      return(IT+NSC+EC)}
  }
  if (category == 3){
    if (income <= 500000)
    {return(0)}
    if (income > 500000 && income <= 1000000) {
      TI <- income - 500000
      IT <- (TI*0.20)
      EC <- IT*0.03
      return(IT+EC)}
    if (income > 1000000 && income <= 5000000) {
      TI <- income - 1000000
      IT <- (TI*0.30) + 100000
      EC <- IT*0.03
      return(IT+EC)}
    if (income > 5000000 && income <= 10000000) {
      TI <- income - 1000000
      IT <- (TI*0.30) + 100000
      SC <- IT*0.10
      MR <- if((income-5000000) < SC) (SC-((income-5000000)-(income-5000000)*.30))else(0)
      NSC <- SC-MR
      EC <- (IT+NSC)*0.03
      return(IT+NSC+EC)}
    if (income > 10000000) {
      TI <- income - 1000000
      IT <- (TI*0.30) + 100000
      SC <- IT*0.15
      MR <- if((income-10000000) < SC) (SC-((income-10000000)-(income-10000000)*.30))else(0)
      NSC <- SC-MR
      EC <- (IT+NSC)*0.03
      return(IT+NSC+EC)}
  }
  if (category == 4){
    if (income <= 250000)
    {return(0)}
    if (income > 250000 && income <= 500000) {
      TI <- income - 300000
      IT <- TI*0.05
      EC <- IT*0.03
      return(IT+EC)}
    if (income > 500000 && income <= 1000000) {
      TI <- income - 500000
      IT <- (TI*0.20) + 12500
      EC <- IT*0.03
      return(IT+EC)}
    if (income > 1000000 && income <= 5000000) {
      TI <- income - 1000000
      IT <- (TI*0.30) + 112500
      EC <- IT*0.03
      return(IT+EC)}
    if (income > 5000000 && income <= 10000000) {
      TI <- income - 1000000
      IT <- (TI*0.30) + 112500
      SC <- IT*0.10
      MR <- if((income-10000000) < SC) (SC-((income-10000000)-(income-10000000)*.30))else(0)
      NSC <- SC-MR
      EC <- (IT+NSC)*0.03
      return(IT+NSC+EC)}
    if (income > 10000000) {
      TI <- income - 1000000
      IT <- (TI*0.30) + 112500
      SC <- IT*0.15
      MR <- if((income-10000000) < SC) (SC-((income-10000000)-(income-10000000)*.30))else(0)
      NSC <- SC-MR
      EC <- (IT+NSC)*0.03
      return(IT+NSC+EC)}
  }
  if (category == 5){
    if (income > 0 && income <= 10000) {
      TI <- income
      IT <- TI*0.10
      EC <- IT*0.03
      return(IT+EC)}
    if (income > 10000 && income <= 20000) {
      TI <- income-10000
      IT <- (TI*0.20) + 1000
      EC <- IT*0.03
      return(IT+EC)}
    if (income > 20000 && income <= 10000000) {
      TI <- income-20000
      IT <- (TI*0.30) + 3000
      EC <- IT*0.03
      return(IT+EC)}
    if (income > 10000000) {
      TI <- income-20000
      IT <- (TI*0.30) + 3000
      SC <- IT*0.12
      MR <- if((income-10000000) < SC) (SC-((income-10000000)-(income-10000000)*.30))else(0)
      NSC <- SC-MR
      EC <- (IT+NSC)*0.03
      return(IT+NSC+EC)}
  }
  if (category == 6){
    if (income > 0 && income <= 10000000) {
      TI <- income
      IT <- (TI*0.30)
      EC <- IT*0.03
      return(IT+EC)}
    if (income > 10000000) {
      TI <- income
      IT <- (TI*0.30)
      SC <- IT*0.12
      MR <- if((income-10000000) < SC) (SC-((income-10000000)-(income-10000000)*.30))else(0)
      NSC <- SC-MR
      EC <- (IT+NSC)*0.03
      return(IT+NSC+EC)}
  }
  stop("ERROR in IIT: category must be either 1 to 6.")
})
