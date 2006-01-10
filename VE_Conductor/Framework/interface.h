/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * File:          $RCSfile: interface.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef TABLE_H
#define TABLE_H

#include <string>
#include <vector>
#include <map>

#include "VE_Installer/include/VEConfig.h"
#include "VE_Conductor/Framework/GeometryDataBuffer.h"


class VE_CONDUCTOR_EXPORTS Interface //: public packable
{

public:

  Interface  ();
  Interface  (const Interface &p);
  ~Interface ();

  void copy (const Interface& p);
   void clear();

  //  virtual bool pack   (std::string &packed);
  //virtual bool unpack (std::string packed);

  long                     getInt      (std::string var, bool *f=NULL);
  double                   getDouble   (std::string var, bool *f=NULL);
  std::string              getString   (std::string var, bool *f=NULL);
  std::vector<long>        getInt1D    (std::string var, bool *f=NULL);
  std::vector<double>      getDouble1D (std::string var, bool *f=NULL);
  std::vector<std::string> getString1D (std::string var, bool *f=NULL);
  GeometryInfoPackage      getGeomInfoPackage (std::string var, bool *f=NULL);
  
  bool getVal (std::string var, long& );
  bool getVal (std::string var, double& );
  bool getVal (std::string var, std::string& );
  bool getVal (std::string var, std::vector<long>& );
  bool getVal (std::string var, std::vector<double>& );
  bool getVal (std::string var, std::vector<std::string>& );

  bool getVal (std::string var, GeometryInfoPackage& );
  
  void setInt      (std::string var, long val);
  void setDouble   (std::string var, double val);
  void setString   (std::string var, std::string val);
  void setInt1D    (std::string var, std::vector<long> val);
  void setDouble1D (std::string var, std::vector<double> val);
  void setString1D (std::string var, std::vector<std::string> val);
  void setGeomInfoPackage (std::string, GeometryInfoPackage );
  
  void setVal (std::string var, long val);
  void setVal (std::string var, double val);
  void setVal (std::string var, std::string val);
  void setVal (std::string var, std::vector<long> val);
  void setVal (std::string var, std::vector<double> val);
  void setVal (std::string var, std::vector<std::string> val);
  void setVal (std::string var, GeometryInfoPackage val);
  
  std::vector<std::string> getInts      ();
  std::vector<std::string> getDoubles   ();
  std::vector<std::string> getStrings   ();
  std::vector<std::string> getInts1D    ();
  std::vector<std::string> getDoubles1D ();
  std::vector<std::string> getStrings1D ();
  std::vector<std::string> getGeomInfoPackages ();

  int _type;
  int _category;
  int _id;
/*  std::map<std::string, long>                      getIntMap()          { return _Int; }
  std::map<std::string, double>                    getDoubleMap()       { return _Double; }
  std::map<std::string, std::string>               getStringMap()       { return _String; }
  std::map<std::string, std::vector<long> >        getIntArrayMap()     { return _Int1D;  }
  std::map<std::string, std::vector<double> >      getDoubleArrayMap()  { return _Double1D;  }
  std::map<std::string, std::vector<std::string> > getStringArrayMap()  { return _String1D; }*/
private:

   std::map<std::string, long >                      _Int;
   std::map<std::string, double >                    _Double;
   std::map<std::string, std::string >               _String;
   std::map<std::string, std::vector<long> >        _Int1D;
   std::map<std::string, std::vector<double> >      _Double1D;
   std::map<std::string, std::vector<std::string> > _String1D;
   std::map<std::string, GeometryInfoPackage >      _GeomInfoPackage;
};

#endif
