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
 * File:          $RCSfile: filename,v $
 * Date modified: $Date: date $
 * Version:       $Rev: 999999 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_DISPLAY_SETTINGS
#define CFD_DISPLAY_SETTINGS
namespace jccl
{
   class Configuration;
}
#include <jccl/Config/ConfigElement.h>

#include "VE_Xplorer/XplorerHandlers/cfdGlobalBase.h"
/*!\file cfdDisplaySettings.h
  VE_XplorerHandlers Interface
  */
/*!\class VE_Xplorer::cfdDisplaySettings
 * This class is used to adjust juggler display properties during runtime
 */
/*!\namespace VE_Xplorer
 * Contains VE_XplorerHandler objects.
 */
namespace VE_Xplorer
{
class VE_XPLORER_EXPORTS cfdDisplaySettings : public cfdGlobalBase
{
public:
   ///Base constructor
   cfdDisplaySettings( void );
   ///Copy Construstor
   //cfdDisplaySettings( const cfdDisplaySettings& input ) { ; }
   ///Destructor
   virtual ~cfdDisplaySettings( void ) { ; }
   ///equal operator
   //cfdDisplaySettings& operator= ( const cfdDisplaySettings& ) { ; }

   ///command array virtual function overriden from globalbase
   virtual bool CheckCommandId( VE_Xplorer::cfdCommandArray * _cfdCommandArray );

   ///in future, multi-threaded apps will make a copy of VjObs_i commandArray
   virtual void UpdateCommand() { ; }
   
private:
   ///This function removes/adds elements to the current display system in
   /// vrjuggler. This can be extended to much more with juggler's rtrc code.
   ///\param remove flag to add/remove elements from an active configuration
   ///\param elements vector of elements to add/remove from an active configuration
   void ChangeDisplayElements( bool remove, 
                               jccl::ConfigElementPtr elements );
   
   ///<A vector that contains current configurations
   jccl::Configuration* configuration;
};
}
#endif
