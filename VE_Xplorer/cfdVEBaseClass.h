/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2004 by Iowa State University
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
 * File:          $RCSfile: cfdExecutive.h,v $
 * Date modified: $Date: 2004-05-18 13:44:18 -0700 (Tue, 18 May 2004) $
 * Version:       $Rev: 382 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_VE_BASECLASS_H
#define CFD_VE_BASECLASS_H

#include "cfdDCS.h"
class cfdModuleGeometry;
class string;
class map;

#include <wx/wx.h>
#include <wx/dc.h>
#include <wx/gdicmn.h>

// Need to create or use this in our stuff
//#include "interface.h"

/*
 * If we're using wx in Dynamic Library format do we 
 * want FL to be in DLL form as well?
 */
#if defined(WXUSINGDLL) && \
    (defined(WXMAKING_PLUGIN_DLL) || defined(WXUSING_PLUGIN_DLL))

#if defined(WXMAKING_PLUGIN_DLL)
    // When building the DLL WXPLUGINDECLSPEC exports classes
#   define WXPLUGIN_DECLSPEC            WXEXPORT
#elif defined(WXUSING_PLUGIN_DLL)
    // When building the DLL WXPLUGINDECLSPEC imports classes
#   define WXPLUGIN_DECLSPEC            WXIMPORT
#endif // defined(WXBUILD_PLUGIN_DLL)

#else
// When building the static library nullify the effect of WXPLUGIN_DECLSPEC
#define WXPLUGIN_DECLSPEC
#endif // WXUSINGDLL && (WXMAKING_PLUGIN_DLL || WXUSING_PLUGIN_DLL)


class WXPLUGIN_DECLSPEC cfdVEBaseClass: public cfdDCS, public wxObject // Inherit from wxBase class to enable string instantiation
{
   DECLARE_DYNAMIC_CLASS( cfdVEBaseClass )

   public:
      cfdVEBaseClass( void );
      ~cfdVEBaseClass( void );

      // Methods to do scene graph manipulations
      // New methods may have to be added later
      virtual void AddSelfToSG( void );
      virtual void RemoveSelfFromSG( void );

      // Change state information for geometric representation
      virtual void MakeTransparent( void );
      virtual void SetColor( float* );
      
      // transform object based 
      virtual void SetTransforms( float*, float*, float* );

      // Implement Gengxun's work by using socket
      // stuff from vtk. This will be used in parallel
      // with implementation of a unit connected to the 
      // computational engine.
      virtual void GetDataFromUnit( void );
      // Basically uses vtkActorToPF to create a geode and 
      // add it to the scene graph. Probably use cfdObject.
      virtual void MakeGeodeByUserRequest( int );

      virtual wxString GetName();
      //This returns the name of the module

      virtual wxString GetDesc();
      //This returns the description of the module, This should be a short description

      virtual void UnPack(Interface* intf);
      //This is the load function of the module, unpack the input string and fill up the UI according to this
      virtual Interface* Pack();

      //This is to unpack the result from the 
      virtual void UnPackResult(Interface * intf) ;
      //This is the save function of the module. 

      virtual void SetID(int id);
   
   private:
      // This needs to be vector of geometry nodes
      cfdModuleGeometry*  geometryNode;

      pfDCS*   worldNode;

   protected:
      // Stuff taken from Plugin_base.h
      // All of Yang's work (REI)
      void RegistVar(string vname, long *var);
      void RegistVar(string vname, double *var);
      void RegistVar(string vname, std::string *var);
      void RegistVar(string vname, std::vector<long> *var);
      void RegistVar(string vname, std::vector<double> *var);
      void RegistVar(string vname, std::vector<std::string> *var);

      Interface mod_pack;

      std::map<std::string, long *>                      _int;
      std::map<std::string, double *>                    _double;
      std::map<std::string, std::string *>               _string;
      std::map<std::string, std::vector<long> * >        _int1D;
      std::map<std::string, std::vector<double> * >      _double1D;
      std::map<std::string, std::vector<std::string> * > _string1D;
};

#endif
