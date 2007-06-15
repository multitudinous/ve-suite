/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

    // class definition for stream class

#ifndef _stream_h_
#define _stream_h_

#include <cstdio>
#include <cstring>
#include <iostream>
#include <vector>
#include <map>
#include "REAL.h"
#include <string>
#include "thermo.h"

class stream
{

public:

   // mutators
   stream(){};
   stream(const thermo& thm, const string stream_file);
   stream(const thermo& thm, REAL& pres0,  std::vector<int>& istream0,
      std::vector<std::string>& frac_typ, std::vector<std::string>& fuel0,
      std::vector< std::vector<std::string> >& spc_nam, 
      std::vector< std::vector<REAL> >& frac, std::vector<REAL>& hform,
	  std::vector<int>& istream1, std::vector<REAL>& str_frac,
	  std::vector<REAL>& temp0);
   ~stream(){};
   void read_stream(const thermo& thm, const string& stream_file);

   // accessors

   const std::vector< std::vector<int> >& get_species() const {return(species);};
   const std::vector< std::vector<REAL> >& get_frac_spec() const {return(frac_spec);};
   const std::vector< std::vector<REAL> >& get_molfrac_spec() const {return(molfrac_spec);};
   std::map< int, int >& get_streams() {return(streams);};// stream # to stream vector location
   const std::vector<int>& get_istreams() const {return(istreams);};
   std::map< int, std::vector<int> >& get_bc_streams() {return(bc_streams);};
   std::map< int, std::map< int, int > >& get_bc_istreams() {return(bc_istreams);}; 
   std::map< int, std::vector<REAL> >& get_frac_stream() {return(frac_stream);}; // mass fraction of streams for bc
   std::map< int, std::vector<REAL> >& get_bc_temp() {return(bc_temp);};
   std::map< int, REAL >& get_bc_dens() {return(bc_dens);};
   const std::vector<bool>& get_lelem() const {return(lelem);};
   const std::vector<bool>& get_fuel() const {return(fuel);};
   const REAL& get_pres() const {return(pres);};
   const std::vector< std::vector<REAL> >& get_mol_atom() const {return(mol_atom);};
   const std::map< int, REAL >& get_hc0() const {return(hc0);}; // stream # map of enthalpy of formation
   const std::map< int, bool >& get_bc_const_temp_eql() const {return(bc_const_temp_eql);};
   std::map< int, bool >& get_bc_const_temp_eql_m() {return(bc_const_temp_eql);};


protected:

   void speciate(const thermo& thm, int& num, int& errcnt,
      std::vector<int>& ispec, std::vector<int>& iel,
      std::vector<REAL>& frc);

   int maxiter;
   std::vector< std::vector<int> > species; // species[i][j] species #'s in thm file for stream i
   std::vector< std::vector<REAL> > frac_spec; // frac_spec[i][j] mass fraction of corresponding species in stream i
   std::vector< std::vector<REAL> > molfrac_spec; // molfrac_spec[i][] mole fra .....
   std::vector< std::vector<REAL> > frac_atom; // frac_atom[i][ie] mass fraction of atom ie per kg of stream i
   std::vector< std::vector<REAL> > mol_atom; // mol_atom[i][ie] kgmol of atom ie per kg of stream i
   std::vector<bool> lelem; // bool for stream having elemental composition defined
   std::vector<bool> fuel; // =true if stream is fuel, =false if oxidant
   std::vector< REAL > mwt; // mixture molecular weight of stream
   std::map< int, int > streams; // stream # to stream vector location
   std::map< int, REAL > hc0; // stream # map of enthalpy of formation
   std::vector<int> istreams; // stream #'s
   std::map< int, std::vector<int> > bc_streams; // bc # to stream vector locations
   std::map< int, std::vector<REAL> > frac_stream; // mass fraction of streams for bc
   std::map< int, std::map< int, int > > bc_istreams; // with each bc# point from stream vector location to bc_stream location
   std::map< int, std::vector<REAL> > bc_temp;
   std::map< int, REAL > bc_dens;
   std::map< int, bool > bc_const_temp_eql;
   REAL pres;
};

#endif
