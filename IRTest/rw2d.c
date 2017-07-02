
double runif();

void
rw2d(int n)
{
    int i = 1;
    int x[10000];
    int y[10000];
    int prev;
    int delta;
    while(i < n) {
	prev = i-1;
        
        if(runif() > .5) 
	    delta = 1L;
	else
	    delta = -1L;

	if(runif() > .5) {
	    x[i] = x[prev] + delta;
	    y[i] = y[prev] ;
	} else {
	    x[i] = x[prev] ;
	    y[i] = y[prev] + delta;
	}
	i = i + 1;
    }
}
