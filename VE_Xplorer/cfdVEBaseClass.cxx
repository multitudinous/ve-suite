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
 * File:          $RCSfile: cfdVEBaseClass.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include "cfdVEBaseClass.h"
#include "cfdModuleGeometry.h"
#include "cfdGroup.h"
#include <string>
#include <map>

IMPLEMENT_DYNAMIC_CLASS( cfdVEBaseClass, wxObject )

// Constructor
cfdVEBaseClass::cfdVEBaseClass( void ) {}


cfdVEBaseClass::cfdVEBaseClass( cfdDCS* veworldDCS )
{
   this->groupNode = new cfdGroup();
   this->_dcs = new cfdDCS();
   this->dataRepresentation = new cfdObjects();
   this->geometryNode = new cfdModuleGeometry( groupNode );
   this->worldDCS = veworldDCS;
   //this->worldDCS->addChild( this->geometryNode->GetPfDCS() );
}

// Destructor
cfdVEBaseClass::~cfdVEBaseClass( void )
{
   delete this->dataRepresentation;
}

// Methods to do scene graph manipulations
// New methods may have to be added later
void cfdVEBaseClass::AddSelfToSG( void )
{
   this->worldDCS->AddChild( this->_dcs );
}

void cfdVEBaseClass::RemoveSelfFromSG( void )
{
   this->worldDCS->RemoveChild( this->_dcs );
}

// Change state information for geometric representation
void cfdVEBaseClass::MakeTransparent( void )
{
   this->geometryNode->SetOpacity( 0.7 );
   this->geometryNode->Update();
}

void cfdVEBaseClass::SetColor( double* color )
{
   this->geometryNode->SetRGBAColorArray( color );
   this->geometryNode->Update();
}
      
// transform object based 
void cfdVEBaseClass::SetTransforms( float* scale, float* rot, float* trans)
{
   this->_dcs->SetTranslationArray( trans );
   this->_dcs->SetScaleArray( scale );
   this->_dcs->SetRotationArray( rot );
}

// Implement Gengxun's work by using socket
// stuff from vtk. This will be used in parallel
// with implementation of a unit connected to the 
// computational engine.
void cfdVEBaseClass::GetDataFromUnit( void )
{
   // Need to get Gengxun's work
   /*std::cout << "this->cfdId = " << geodeEnumToString(this->cfdId) << std::endl;
   this-> sock = vtkSocketCommunicator::New();
   this-> sock->WaitForConnection(33000);

   std::cout << "[DBG] VE_Xplorer is connected to the port 33000 "<< std::endl;
   

   vprDEBUG(vprDBG_ALL,1)
         <<" UPDATE_INTERACTIVE_DESIGN " << this->Interactive_state;
   
   vtkUnstructuredGrid* ugrid = vtkUnstructuredGrid::New();
   
   if (!this->sock->Receive(ugrid,1,9))
   {
      std::cerr << " cfdCalculator side error :: Error receiving data." << std::endl;
      if (this->sock)
      {
         this->sock->CloseConnection();
         this->sock->Delete();
         this->sock = NULL;

      }

      ugrid->Delete();

   }

   std::cout << "[DBG] Receiving ugrid data..." << std::endl;
   
   
   
   if( this -> sock)
   {
      std::cout << "[DBG] testing if the sock is still connected" << std::endl;
      this->sock->CloseConnection();
      this->sock->Delete();
      this->sock = NULL;

   }*/
}
// Basically uses vtkActorToPF to create a geode and 
// add it to the scene graph. Probably use cfdObject.
void cfdVEBaseClass::MakeGeodeByUserRequest( int )
{
   this->dataRepresentation->UpdatecfdGeode();
}

//This returns the name of the module
wxString cfdVEBaseClass::GetName( void )
{
   return this->_objectName;
}

//This returns the description of the module, This should be a short description
wxString cfdVEBaseClass::GetDesc( void )
{
   return this->_objectDescription;
}


//This is the load function of the module, unpack the input string and fill up the UI according to this
void cfdVEBaseClass::UnPack(Interface* intf)
{
   vector<string> vars;
  
  map<string, long *>::iterator iteri;
  map<string, double *>::iterator iterd;
  map<string, string *>::iterator iters;
  map<string, vector<long> *>::iterator itervi;
  map<string, vector<double> *>::iterator itervd;
  map<string, vector<string> *>::iterator itervs;
  
  unsigned int i;
  long temp;

  mod_pack = *intf;
  vars = mod_pack.getInts();
  for (i=0; i<vars.size(); i++)
    {
      iteri =_int.find(vars[i]);
      if (iteri!=_int.end())
	mod_pack.getVal(vars[i], *(iteri->second));
      else if (vars[i]=="XPOS")
	{
	  mod_pack.getVal("XPOS", temp);
	  //	  printf("xpos %ld\n", temp);
	  //pos.x = temp;
     pos_x = temp;
	}
      else if (vars[i]=="YPOS")
	{
	  //	  printf("ypos %ld\n", temp);
	  mod_pack.getVal("YPOS", temp);
	  //pos.y = temp;
     pos_y = temp;
	}
    }

  vars = mod_pack.getDoubles();
  for (i=0; i<vars.size(); i++)
    {
      iterd =_double.find(vars[i]);
      if (iterd!=_double.end())
	mod_pack.getVal(vars[i], *(iterd->second));
    }  
  
  vars = mod_pack.getStrings();
  for (i=0; i<vars.size(); i++)
    {
      iters =_string.find(vars[i]);
      if (iters!=_string.end())
	mod_pack.getVal(vars[i], *(iters->second));
    }

  vars = mod_pack.getInts1D();
  for (i=0; i<vars.size(); i++)
    {
      itervi =_int1D.find(vars[i]);
      if (itervi!=_int1D.end())
	mod_pack.getVal(vars[i], *(itervi->second));
    }

   vars = mod_pack.getDoubles1D();
   for (i=0; i<vars.size(); i++)
   {
      itervd =_double1D.find(vars[i]);
      if (itervd!=_double1D.end())
	      mod_pack.getVal(vars[i], *(itervd->second));
   }

   vars = mod_pack.getStrings1D();
   for (i=0; i<vars.size(); i++)
   {
      itervs =_string1D.find(vars[i]);
      if (itervs!=_string1D.end())
         mod_pack.getVal(vars[i], *(itervs->second));
   }
}

Interface* cfdVEBaseClass::Pack()
{  
   string result;
  
   map<string, long *>::iterator iteri;
   map<string, double *>::iterator iterd;
   map<string, string *>::iterator iters;
   map<string, vector<long> *>::iterator itervi;
   map<string, vector<double> *>::iterator itervd;
   map<string, vector<string> *>::iterator itervs;


   //printf("mod id : %d\n", mod_pack._id);
   //mod_pack.setVal("XPOS",long (pos.x));
   //mod_pack.setVal("YPOS",long (pos.y));
   mod_pack.setVal("XPOS",long (pos_x));
   mod_pack.setVal("YPOS",long (pos_y));
  
   for(iteri=_int.begin(); iteri!=_int.end(); iteri++)
      mod_pack.setVal(iteri->first, *(iteri->second));

   for(iterd=_double.begin(); iterd!=_double.end(); iterd++)
      mod_pack.setVal(iterd->first, *(iterd->second));

   for(iters=_string.begin(); iters!=_string.end(); iters++)
      mod_pack.setVal(iters->first, *(iters->second));

   for(itervi=_int1D.begin(); itervi!=_int1D.end(); itervi++)
      mod_pack.setVal(itervi->first, *(itervi->second));

   for(itervd=_double1D.begin(); itervd!=_double1D.end(); itervd++)
      mod_pack.setVal(itervd->first, *(itervd->second));

   for(itervs=_string1D.begin(); itervs!=_string1D.end(); itervs++)
      mod_pack.setVal(itervs->first, *(itervs->second));

   //mod_pack.pack(result);
  
   //wxString wxstr = result.c_str();
   return &mod_pack ;//wxstr;
}

//This is to unpack the result from the 
void cfdVEBaseClass::UnPackResult(Interface * intf)
{
}

// Set the id for a particular module
void cfdVEBaseClass::SetID(int id)
{
}


   
// Stuff taken from Plugin_base.h
// All of Yang's work (REI)
/////////////////////////////////////////////////////////////////////////////
void cfdVEBaseClass::RegistVar(string vname, long *var)
{
  _int[vname]=var;
}

void cfdVEBaseClass::RegistVar(string vname, double *var)
{
  _double[vname]=var;
}

void cfdVEBaseClass::RegistVar(string vname, string *var)
{
  _string[vname]=var;
}

void cfdVEBaseClass::RegistVar(string vname, vector<long> *var)
{
  _int1D[vname]=var;
}

void cfdVEBaseClass::RegistVar(string vname, vector<double> *var)
{
  _double1D[vname]=var;
}

void cfdVEBaseClass::RegistVar(string vname, vector<string> *var)
{
  _string1D[vname]=var;
}
