#include <Rcpp.h>

typedef std::vector<double> rec;
typedef std::vector<rec> recs;

bool Comp(const rec& a, const rec& b) {
  bool res = a[1] < b[1];
  if(a[1]==b[1]) res = (a[3]) < (b[3]);
  return res;
}

// [[Rcpp::export]]
Rcpp::List lastdose_impl(Rcpp::NumericVector id,
                         Rcpp::NumericVector time,
                         Rcpp::NumericVector amt,
                         Rcpp::NumericVector evid,
                         Rcpp::NumericVector addl,
                         Rcpp::NumericVector ii,
                         Rcpp::NumericVector fill,
                         Rcpp::LogicalVector back_calc) {

  bool use_fill = !back_calc[0];
  bool has_addl = false;
  if(addl.size() > 0) has_addl = true;
  std::vector<double> idn;
  std::vector<int> idstart;
  std::vector<int> idend;
  idn.reserve(1000);
  idstart.reserve(1000);
  idend.reserve(1000);
  double lastid = -1E9;
  for(int i = 0; i < id.size(); i++) {
    if(id[i] != lastid) {
      idn.push_back(id[i]);
      lastid = id[i];
      idstart.push_back(i);
      if(i > 0) idend.push_back(i-1);
    }
  }
  idend.push_back(id.size()-1);
  int crow = 0;
  Rcpp::NumericVector tad(id.size());
  Rcpp::NumericVector ldos(id.size());
  std::vector<double> told;
  told.assign(idn.size(),-1.0);
  for(int i = 0; i < idn.size(); i++) {
    double this_idn = idn[i];
    double max_time = time[idend[i]];
    recs this_id;
    this_id.reserve((idend[i] - idstart[i])*3);
    bool found_dose = false;
    for(int j = idstart[i]; j <= idend[i]; j++) {
      std::vector<double> this_rec;
      this_rec.resize(5);
      this_rec[0] = this_idn;
      this_rec[1] = time[j];
      this_rec[2] = 0;
      this_rec[3] = amt[j];
      this_rec[4] = evid[j];
      if(!found_dose && (evid[j]==1 || evid[j]==4)) {
        found_dose = true;
        told[i] = time[j];
      }
      this_id.push_back(this_rec);
      if(has_addl && (addl[j] > 0) && (evid[j]==1 || evid[j]==4)) {
        for(int k = 0; k < addl[j]; k++) {
          rec addl_rec(5);
          addl_rec[0] = this_idn;
          addl_rec[1] = time[j] + ii[j]*double(k+1);
          addl_rec[2] = 1;
          addl_rec[3] = amt[j];
          addl_rec[4] = evid[j];
          if(addl_rec[1] >= (max_time)) break;
          this_id.push_back(addl_rec);
        }
      }
    } // END THIS ID
    std::sort(this_id.begin(), this_id.end(), Comp);
    double last_dose = 0;
    bool had_dose = false;
    bool no_dose = told[i] == -1;
    double last_time = 0;
    for(int m = 0; m < this_id.size(); m++) {
      if(this_id[m][4] ==1 | this_id[m][4]==4) {
        had_dose = true;
        last_dose = this_id[m][3];
        last_time = this_id[m][1];
      }
      if((this_id[m])[2]==0) {
        if(had_dose) {
          tad[crow] = this_id[m][1] - last_time;
        } else {
          tad[crow] = (use_fill || no_dose) ? fill[0] : (this_id[m][1] - told[i]);
        }
        ldos[crow] = last_dose;
        crow++;
      }
    }
  }
  Rcpp::List ans;
  ans["tad"] = tad;
  ans["ldos"] = ldos;
  return ans;
}





