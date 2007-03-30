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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef PACKAGE_H
#define PACKAGE_H

#include "VE_Conductor/Network/interface.h"
#include <vector>
#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/parsers/XercesDOMParser.hpp>
#include <xercesc/dom/DOM.hpp>
#include <xercesc/util/XMLString.hpp>
#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/framework/MemBufInputSource.hpp>
#include <xercesc/framework/LocalFileFormatTarget.hpp>
//#include <iostream>
#include <string>

XERCES_CPP_NAMESPACE_USE
class VE_CONDUCTOR_EXPORTS Package
{
public:
   Package();
   ~Package();
   Package( const Package& input )
   {
      this->intfs = input.intfs;
      
      this->intfsnum = input.intfsnum;
      this->packname = input.packname; 
      this->system_id = input.system_id;
   }

   Package& operator= ( const Package& input )
   {
      if ( this != &input )
      {
         this->intfs = input.intfs;

         this->intfsnum = input.intfsnum;
         this->packname = input.packname; 
         this->system_id = input.system_id;
      }
      return *this;
   }

   void SetPackName(const char* name) { packname = std::string(name); };
   const char* GetPackName() { return packname.c_str(); };

   void SetIntfsNum(int );
   int GetIntfsNum();

   void SetSysId(const char * fname) { system_id = std::string(fname); };
   const char* GetSysId() { return system_id.c_str(); };

   bool Load(); //parse the file specified by the system id and build up all the interfaces

   bool Load(const char *mem, int len); //Parse from the memory block and build up all the interfaces

   bool Save(); //Write well formed XML to the file specified by the system id
  
   const char* Save(bool &rv); //Write well formed XML to the string it returns
  
   std::vector<Interface> GetInterfaceVector( void );

protected:

   int intfsnum;
   std::string packname; //The package name, goes to the name attrib of the element package
   std::string system_id; //The file name of the xml file or a fake filename used for parsing the memory buffer input source

   void FillIntfs( XERCES_CPP_NAMESPACE_QUALIFIER DOMDocument *doc );
   XERCES_CPP_NAMESPACE_QUALIFIER DOMDocument* BuildFromIntfs();

private:
   std::vector<Interface> intfs; //The vector of the interfaces packed in this package.
};

#endif
