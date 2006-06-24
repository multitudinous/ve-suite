/*************** <auto-copyright.pl BEGIN do not edit this line> **************

 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *   Headed by: Kenneth Mark Bryden, Ph.D.
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
 * File:          $RCSfile: cfdSoundHandler.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_SOUNDHANDLER
#define CFD_SOUNDHANDLER

#include <string>
#include <vector>
#include "VE_Xplorer/XplorerHandler/cfdGlobalBase.h"

namespace VE_Xplorer
{
   class cfdCommandArray;
   class cfdSound;
   class cfdReadParam;
}

namespace VE_Xplorer
{
class VE_XPLORER_EXPORTS cfdSoundHandler : public cfdGlobalBase
{
public:
   cfdSoundHandler( std::string );
   ~cfdSoundHandler();

   // Used to read parameter file and configure objects
   void CreateObjects( void );

   // compare VjObs_i commandArray with its child's value
   virtual bool CheckCommandId( cfdCommandArray * _cfdCommandArray );


   // in future, multi-threaded apps will make a copy of 
   // VjObs_i commandArray
   virtual void UpdateCommand();

   int GetNumberOfSounds( void );
   std::string GetSoundFilename( int );
   std::vector< cfdSound* > GetSounds( void );
private:
   std::string _param;
   cfdReadParam* _readParam;
   std::vector< cfdSound* > sounds;
   //cfdCommandArray* commandArray;
};
}
#endif
