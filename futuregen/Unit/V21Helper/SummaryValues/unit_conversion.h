
#ifndef UNIT_CONVERSION_H
#define UNIT_CONVERSION_H

#include <map>
#include <string>
#pragma warning (disable:4786)
///////////////////////////////////////////////////////////////////////////////////////

class dummy {

public:

  dummy () {};
  dummy (const dummy& d) {
    for(int i=0;i<3;i++)units[i]=d.units[i];
    memcpy(factors,d.factors,3*sizeof(float));
  }

  dummy& operator= (const dummy& d) {
    // check for self assign.
    if(this==&d)
      return(*this); 
    for(int i=0;i<3;i++)
      units[i]=d.units[i]; 
    memcpy(factors,d.factors,3*sizeof(float));
    return(*this);
  }

  std::string units[3];
  float       factors[3];

};

///////////////////////////////////////////////////////////////////////////////////////

typedef std::pair<std::string, float> UnitPair;
typedef std::pair<std::string, dummy> TablePair;

enum unit_type {si=0, metric=1, english=2};

///////////////////////////////////////////////////////////////////////////////////////

class UnitConverter {

public:

  UnitConverter  ();
  ~UnitConverter ();

  UnitPair convert(const std::string& orig_units, const float& value,	 const unit_type& units_out) const;
  UnitPair convert_temp (const std::string& orig_units, const float& value,
			 const unit_type& units_out) const;

protected:
 
  void add_dummy (dummy d);

  std::map<std::string, dummy> conversion_table;

};

///////////////////////////////////////////////////////////////////////////////////////

#endif
