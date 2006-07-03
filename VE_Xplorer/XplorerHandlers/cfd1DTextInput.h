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
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_1DTEXTINPUT_H
#define CFD_1DTEXTINPUT_H

#include <string>
namespace VE_SceneGraph
{
   class cfdGeode;
}
class  vtkActor;
#include "VE_Xplorer/SceneGraph/cfdDCS.h"

namespace VE_Xplorer
{
   class cfd1DTextInput : public VE_SceneGraph::cfdDCS
   {
      public:

         cfd1DTextInput( void );

         ~cfd1DTextInput( void );

         VE_SceneGraph::cfdDCS* getpfDCS( void );

         void SetTransforms( float [ 3 ], float [ 3 ], float [ 3 ] );

         void SetFilename( std::string );

         void Update( void );
   
         void UpdateTextColor( double , double , double );

      private:
   
         std::string text;
         vtkActor*   actor;
         //cfdDCS*      DCS;
         VE_SceneGraph::cfdGeode* geode;  
   };
}
#endif
