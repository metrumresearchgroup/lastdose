$SET req=""
$CMT X
$ODE
dxdt_X=0;
$PREAMBLE
capture LDOS = 0;
$TABLE
if(NEWIND <= 1) LDOS = 0;
if(EVID==1 | EVID==4) LDOS = self.amt;
capture TAD = self.tad();

