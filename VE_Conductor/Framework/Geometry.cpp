/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2005 by Iowa State University
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
 * File:          $RCSfile: filename,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Conductor/Framework/Geometry.h"
#include "VE_Xplorer/cfdEnum.h"
#include "VE_Conductor/Framework/interface.h"

#include <iostream>
#include <string>
#include <cmath>

Geometry::Geometry( int id )
{
   this->cur_moduleid = id;
   mod_pack = new Interface();
}

void Geometry::SetID(int id)
{
   mod_pack->_id =id;
}

Geometry::~Geometry()
{
   delete mod_pack;
}

Geometry::Geometry( const Geometry& input )
{
   this->_geominfopackagemap = _geominfopackagemap;
   this->mod_pack = new Interface( (*input.mod_pack) );
}

Geometry& Geometry::operator= ( const Geometry& input )
{
   if ( this != &input )
   {
      this->_geominfopackagemap = _geominfopackagemap;

      delete this->mod_pack;
      this->mod_pack = new Interface( (*input.mod_pack) );

      this->cur_moduleid = input.cur_moduleid;
      this->cur_modulename = input.cur_modulename; 
   }
   return *this;
}

void Geometry::UnPack( Interface* intf )
{

   std::vector<std::string> vars;

   std::vector<GeometryInfoPackage> templist;

   GeometryInfoPackage temppackage;
   
   unsigned int i;

   vars = intf->getGeomInfoPackages();
   bool rv =true;
   for(i=0;i<vars.size();i++)
   {
      temppackage = intf->getGeomInfoPackage(vars[i], &rv);
      templist.push_back(temppackage);
   }

   geometryDataBuffer->SetCurrentGeomInfoList(templist);
}

Interface* Geometry::Pack( void )
{
     
   SetCurModuleGeomInfoPackage();

   mod_pack->_type = 2; //Module
   mod_pack->_category = 1; // normal modules
   mod_pack->_id = cur_moduleid;

   std::map<std::string, GeometryInfoPackage>::iterator iter;
   
   for(iter=_geominfopackagemap.begin();iter!=_geominfopackagemap.end();iter++)
   {
      mod_pack->setGeomInfoPackage(iter->first, iter->second);

   }

   return mod_pack;
}

void Geometry::SetCurModuleGeomInfoPackage()
{
  
   _geominfopackagemap.clear();
   std::vector<GeometryInfoPackage> locallist = geometryDataBuffer->GetCurrentGeomInfoList();
   for(unsigned int i=0; i<locallist.size(); i++)
   {
      _geominfopackagemap.insert(std::make_pair(locallist[i].GetGeomName(), locallist[i]));

   }
}

void Geometry::SetGeometryDataBuffer( GeometryDataBuffer* input )
{
   geometryDataBuffer = input;
}
