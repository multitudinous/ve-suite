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

#include "cfdVEBaseClass.h"

// Constructor
cfdVEBaseClass( void )
{
   this->dataRepresentation = new cfdObjects();
}

// Destructor
~cfdVEBaseClass( void )
{
   delete this->dataRepresentation;
}

// Methods to do scene graph manipulations
// New methods may have to be added later
void AddSelfToSG( void )
{

}

virtual void RemoveSelfFromSG( void );

// Change state information for geometric representation
virtual void MakeTransparent( void );
virtual void SetColor( float* );
      
// transform object based 
virtual void SetTransforms( float* scale, float* rot, float* trans)
{
   this->SetTranslationArray( trans );
   this->SetScaleArray( scale );
   this->SetRotationArray( rot );
}

// Implement Gengxun's work by using socket
// stuff from vtk. This will be used in parallel
// with implementation of a unit connected to the 
// computational engine.
virtual void GetDataFromUnit( void )
{
   // Need to get Gengxun's work
}
// Basically uses vtkActorToPF to create a geode and 
// add it to the scene graph. Probably use cfdObject.
virtual void MakeGeodeByUserRequest( int )
{
   this->dataRepresentation->UpdateGeode();
}

//This returns the name of the module
virtual wxString GetName( void )
{
   return this->_objectName;
}

//This returns the description of the module, This should be a short description
virtual wxString GetDesc( void )
{
   return this->_objectDescription;
}


//This is the load function of the module, unpack the input string and fill up the UI according to this
virtual void UnPack(Interface* intf)
{
}

virtual Interface* Pack()
{
}

//This is to unpack the result from the 
virtual void UnPackResult(Interface * intf)
{
}

// Set the id for a particular module
virtual void SetID(int id)
{
}
   
// Stuff taken from Plugin_base.h
// All of Yang's work (REI)
void RegistVar(string vname, long *var);
void RegistVar(string vname, double *var);
void RegistVar(string vname, std::string *var);
void RegistVar(string vname, std::vector<long> *var);
void RegistVar(string vname, std::vector<double> *var);
void RegistVar(string vname, std::vector<std::string> *var);
