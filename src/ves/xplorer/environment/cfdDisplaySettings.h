/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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
 *************** <auto-copyright.rb END do not edit this line> ***************/
#ifndef CFD_DISPLAY_SETTINGS
#define CFD_DISPLAY_SETTINGS
namespace jccl
{
class Configuration;
}
#include <jccl/Config/ConfigElement.h>

#include <utility>

#include <ves/xplorer/GlobalBase.h>
/*!\file cfdDisplaySettings.h
  VE_XplorerHandlers Interface
  */
/*!\class ves::xplorer::cfdDisplaySettings
 * This class is used to adjust juggler display properties during runtime
 */
/*!\namespace ves { namespace xplorer
 * Contains VE_XplorerHandler objects.
 */
namespace ves
{
namespace xplorer
{
class VE_XPLORER_EXPORTS cfdDisplaySettings : public GlobalBase
{
public:
    ///Base constructor.
    cfdDisplaySettings( void );
    ///Copy Construstor
    //cfdDisplaySettings( const cfdDisplaySettings& input ) { ; }
    ///Destructor.
    virtual ~cfdDisplaySettings( void )
    {
        ;
    }
    ///equal operator
    //cfdDisplaySettings& operator= ( const cfdDisplaySettings& ) { ; }

    ///Process the display command
    virtual void ProcessCommand();

    ///in future, multi-threaded apps will make a copy of VjObs_i commandArray.
    virtual void UpdateCommand()
    {
        ;
    }

    ///Obtains the screen resolution.
    std::pair< int, int > GetScreenResolution( void );
    ///Sets screen corner values.
    std::map< std::string, double > GetScreenCornerValues( void );

private:
    ///This function removes/adds elements to the current display system in
    /// vrjuggler. This can be extended to much more with juggler's rtrc code.
    ///\param remove flag to add/remove elements from an active configuration
    ///\param elements vector of elements to add/remove from an active configuration
    void ChangeDisplayElements( bool remove, jccl::ConfigElementPtr elements );


    jccl::Configuration* configuration;///<A vector that contains current configurations.

    int xSize;///<Screen size in pixels for x direction.
    int ySize;///<Screen size in pixels for y direction.

    double newXmin;///<Sets new x min value for screen corner.
    double newXmax;///<Sets new x max value for screen corner.
    double newYmin;///<Sets new y min value for screen corner.
    double newYmax;///<Sets new y max value for screen corner.
    double newZval;///<Sets new z value for screen.
};
}
}
#endif
