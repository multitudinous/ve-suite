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
 * File:          $RCSfile: cfdExecutiveConfiguration.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "cfdExecutiveConfiguration.h"
#include "fileIO.h"
#include <iostream>

cfdExecutiveConfiguration::cfdExecutiveConfiguration( void )
{
   std::cout << "|   Enter cfdExecutive parameter filename :  " << std::endl;
   std::cin >> this->_paramFilename;
   
   //std::cout << "this->_paramFilename : " << this->_paramFilename << std::endl;
   if ( ! fileIO::isFileReadable( (char*)this->_paramFilename.c_str() ) ) 
   {
      std::cerr << "\nError: Could not open the input file " 
                << this->_paramFilename << " !" << std::endl;
      while( !fileIO::isFileReadable( (char*)this->_paramFilename.c_str() ) )
      {
         std::cout << "\nEnter correct filename: " << std::endl; 
         std::cin >> this->_paramFilename;        
      }
   }
}

cfdExecutiveConfiguration::~cfdExecutiveConfiguration( void )
{
}

std::string cfdExecutiveConfiguration::GetParameterFilename( void )
{
   return this->_paramFilename;
}
