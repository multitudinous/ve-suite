#ifndef TABLE_H
#define TABLE_H


//#include "packable.h"

#ifdef _WIN32
#pragma warning(disable : 4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)
#endif

#include <string>
#include <vector>
#include <map>

//#define DELIM  char(0xA9)

class Interface //: public packable
{

public:

  Interface  ();
  Interface  (const Interface &p);
  ~Interface ();

  void copy (const Interface& p);

  //  virtual bool pack   (std::string &packed);
  //virtual bool unpack (std::string packed);

  void clear();

  long                     getInt      (std::string var, bool *f=NULL);
  double                   getDouble   (std::string var, bool *f=NULL);
  std::string              getString   (std::string var, bool *f=NULL);
  std::vector<long>        getInt1D    (std::string var, bool *f=NULL);
  std::vector<double>      getDouble1D (std::string var, bool *f=NULL);
  std::vector<std::string> getString1D (std::string var, bool *f=NULL);
 
  bool getVal (std::string var, long &val)
    { bool b; val = getInt(var, &b); return b; }
  bool getVal (std::string var, double &val)
    { bool b; val = getDouble(var, &b); return b; }
  bool getVal (std::string var, std::string &val)
    { bool b; val = getString(var, &b); return b; }
  bool getVal (std::string var, std::vector<long> &val)
    { bool b; val = getInt1D(var, &b); return b; }
  bool getVal (std::string var, std::vector<double> &val)
    { bool b; val = getDouble1D(var, &b); return b; }
  bool getVal (std::string var, std::vector<std::string> &val)
    { bool b; val = getString1D(var, &b); return b; }

  void setInt      (std::string var, long val)                     { _Int[var] = val; }
  void setDouble   (std::string var, double val)                   { _Double[var] = val; }
  void setString   (std::string var, std::string val)              { _String[var] = val; }
  void setInt1D    (std::string var, std::vector<long> val)        { _Int1D[var] = val; }
  void setDouble1D (std::string var, std::vector<double> val)      { _Double1D[var] = val; }
  void setString1D (std::string var, std::vector<std::string> val) { _String1D[var] = val; }

  void setVal (std::string var, long val)                     { setInt(var, val); }
  void setVal (std::string var, double val)                   { setDouble(var, val); }
  void setVal (std::string var, std::string val)              { setString(var, val); }
  void setVal (std::string var, std::vector<long> val)        { setInt1D(var, val); }
  void setVal (std::string var, std::vector<double> val)      { setDouble1D(var, val); }
  void setVal (std::string var, std::vector<std::string> val) { setString1D(var, val); }

  std::vector<std::string> getInts      ();
  std::vector<std::string> getDoubles   ();
  std::vector<std::string> getStrings   ();
  std::vector<std::string> getInts1D    ();
  std::vector<std::string> getDoubles1D ();
  std::vector<std::string> getStrings1D ();

  int _type;
  int _category;
  int _id;
  
protected:

  std::map<std::string, long>                      _Int;
  std::map<std::string, double>                    _Double;
  std::map<std::string, std::string>               _String;
  std::map<std::string, std::vector<long> >        _Int1D;
  std::map<std::string, std::vector<double> >      _Double1D;
  std::map<std::string, std::vector<std::string> > _String1D;

};

#endif
