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
#ifndef CFD_INTERACTIVEGEOMETRY_H
#define CFD_INTERACTIVEGEOMETRY_H
// This class will create one train and then will clone trains 
// as neccessary 
// This class will have a vector geometry files
// THis class will keep track of which modules 
// need to be swapped and how many duplivates of the base 
// modules need to be created or destroyed

#include <string>
#include <vector>
#include <utility>

class cfdGroup;
class cfdDCS;
class cfdModuleGeometry;
class cfdExecutive;

class cfdInteractiveGeometry
{
   public:
      cfdInteractiveGeometry( std::string, cfdGroup* );
      ~cfdInteractiveGeometry( void );

      void GetNumberOfTrains( void );
      void CreateGeometryList( void );
      void Update( std::string, cfdExecutive* );

   private:
      cfdGroup* _masterNode;
      cfdGroup* _trainNode;

      std::string _param;
      int _numberOfModules;
      std::pair < cfdModuleGeometry*, cfdModuleGeometry* > _geomPair;

      std::vector < std::pair < cfdModuleGeometry*, cfdModuleGeometry* > > _swappableGeometry;
      std::vector < cfdModuleGeometry* > _moduleGeometry;
      std::vector < cfdDCS* > _trainDCS;
};

#endif
