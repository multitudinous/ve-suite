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
 * File:          $RCSfile: cfd1DTextInput.h,v $
 * Date modified: $Date: 2004/03/23 16:29:12 $
 * Version:       $Revision: 1.3 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_1DTEXTINPUT_H
#define CFD_1DTEXTINPUT_H

#include <vector>
#include <string>

//class pfDCS;
class vtkActor;
class pfGeode;

#include "cfdDCS.h"

class cfd1DTextInput : public cfdDCS
{
   public:

      cfd1DTextInput( void );
      //cfd1DTextInput( cfd1DTextInput* );

      ~cfd1DTextInput( void );

      pfDCS* getpfDCS( void );

      void SetTransforms( float [ 3 ], float [ 3 ], float [ 3 ] );
      void SetFilename( std::string );
      void Update( void );
   
      std::string text;
   private:
   
      //float scale[ 3 ];
      //float trans[ 3 ];
      //float rot[ 3 ];

      
      vtkActor*   actor;
      //pfDCS*      DCS;
      pfGeode*    geode;  
};

#endif
