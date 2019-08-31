#include <Rcpp.h>

class  record {
public:
  record(double id_, double time_, double amt_, int evid_);
  double id;
  double time;
  double amt;
  int evid;
  bool from_data;
};

record::record(double id_, double time_, double amt_, int evid_) {
  id = id_;
  time = time_;
  amt = amt_;
  evid = evid_;
}

typedef std::vector<record> recs;

bool Comp1(const record& a, const record& b) {
  bool res = a.time == b.time ? a.amt < b.amt : a.time < b.time;
  return res;
}

bool Comp2(const record& a, const record& b) {
  bool res = a.time == b.time ? b.amt < a.amt : a.time < b.time;
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
                         Rcpp::LogicalVector back_calc,
                         Rcpp::LogicalVector sort1) {

  bool use_comp1 = sort1[0];
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
  int nrows = id.size();
  for(int i = 0; i < nrows; ++i) {
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
  int nid = idn.size();
  for(int i = 0; i < nid; ++i) {
    double this_idn = idn[i];
    double max_time = time[idend[i]];
    recs this_id;
    this_id.reserve((idend[i] - idstart[i])*3);
    bool found_dose = false;
    for(int j = idstart[i]; j <= idend[i]; ++j) {
      record this_rec(this_idn,time[j],amt[j],evid[j]);
      this_rec.from_data = true;
      if(!found_dose && ((evid[j]==1) || (evid[j]==4))) {
        found_dose = true;
        told[i] = time[j];
      }
      this_id.push_back(this_rec);
      if(has_addl && (addl[j] > 0) && ((evid[j]==1) || (evid[j]==4))) {
        for(int k = 0; k < addl[j]; ++k) {
          record addl_rec(this_idn,0.0,amt[j],evid[j]);
          addl_rec.from_data = false;
          addl_rec.time = time[j] + ii[j]*double(k+1);
          if(addl_rec.time >= (max_time)) break;
          this_id.push_back(addl_rec);
        }
      }
    }
    if(use_comp1) {
      std::sort(this_id.begin(), this_id.end(), Comp1);
    } else {
      std::sort(this_id.begin(), this_id.end(), Comp2);
    }
    double last_dose = 0;
    bool had_dose = false;
    bool no_dose = told[i] == -1;
    double last_time = 0;
    for(recs::const_iterator it = this_id.begin(); it !=this_id.end(); ++it) {
      if((it->evid ==1) || (it->evid==4)) {
        had_dose = true;
        last_dose = it->amt;
        last_time = it->time;
      }
      if(it->from_data) {
        if(had_dose) {
          tad[crow] = it->time - last_time;
        } else {
          tad[crow] = (use_fill || no_dose) ? fill[0] : (it->time - told[i]);
        }
        ldos[crow] = last_dose;
        ++crow;
      }
    }
  }
  Rcpp::List ans;
  ans["tad"] = tad;
  ans["ldos"] = ldos;
  return ans;
}
