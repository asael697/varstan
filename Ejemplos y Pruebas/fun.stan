functions{
  vector idif(vector x, real x0 ){
    int n = num_elements(x);
    vector [n+1] y;
    y[1] = x0;
    for(i in 2:(n+1)){
      y[i] = y[i-1] +x[i-1];
    }
    return y;
  }
  vector complete(vector x, int d, real x0){
    int n = num_elements(x);
    vector [d] y;
    if(n < d){
      for(i in 1:d){
        if(i <= n) y[i] = x[i];
        else y[i] = x0;
      }
    }
    else{
      for(i in 1:d)
        y[i] = x[i];
    }
    return y;
  }
  vector inv_dif(vector x, int d,vector x0);
  vector inv_dif(vector x, int d,vector x0){
    int n = num_elements(x);
    vector[d] x1 = complete(x0,d,0.0);
    vector[n+1] y = idif(x,x1[d]);
    if((d-1) == 0 )
      return y;
    else
      return inv_dif(y,d-1,x1);
  }
}