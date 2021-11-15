#pragma once
#ifndef GUARD_OptimizationProblem_h
#define GUARD_OptimizationProblem_h

#include "Package.h"


//CLASS DECLARATION.
class OptimizationProblem
{
public:
  //Constructor
  OptimizationProblem(){};
  OptimizationProblem(std::size_t nrow, std::size_t ncol, std::size_t ncell){
    _obj.reserve(ncol);
    _A_i.reserve(ncell);
    _A_j.reserve(ncell);
    _A_x.reserve(ncell);
    _rhs.reserve(ncol);
    _vtype.reserve(ncol);
    _lb.reserve(nrow);
    _ub.reserve(nrow);
    _sense.reserve(nrow);
    _name.reserve(ncol);
    _id_pow_variables.reserve(nrow);
    _id_variables.reserve(nrow);
  };

  // deconstructor
  ~OptimizationProblem(){};

  // fields
  std::string _modelsense;
  std::vector<double> _obj;
  std::vector<std::size_t> _A_i;
  std::vector<std::size_t> _A_j;
  std::vector<double> _A_x;
  std::vector<double> _rhs;
  std::vector<std::string> _vtype;
  std::vector<double> _lb;
  std::vector<double> _ub;
  std::vector<std::string> _sense;
  std::vector<std::string> _name;
  std::vector<double> _id_pow_variables;
  std::vector<double> _id_variables;
  int _boundary_size;

  // methods
  inline const std::size_t nrow() const {
    return(_rhs.size());
  }

  inline const std::size_t ncol() const {
    return(_obj.size());
  }

  inline const std::size_t ncell() const {
    return(_A_x.size());
  }

  inline const Rcpp::List A() const {
    return(Rcpp::List::create(
        Rcpp::Named("i")=_A_i,
        Rcpp::Named("j")=_A_j,
        Rcpp::Named("x")=_A_x));
  }
};

#endif
