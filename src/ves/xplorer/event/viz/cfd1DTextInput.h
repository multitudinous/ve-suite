/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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
#ifndef CFD_1DTEXTINPUT_H
#define CFD_1DTEXTINPUT_H

#include <ves/xplorer/scenegraph/DCS.h>
#include <ves/xplorer/scenegraph/Geode.h>

#ifdef _OSG
#include <osg/ref_ptr>
#elif _PERFORMER
#endif

#include <string>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
class Geode;
}
}
}

class  vtkActor;

namespace ves
{
namespace xplorer
{
/*!\file cfd1DTextInput.h
cfd1DTextInput API
*/
/*!\class ves::xplorer::cfd1DTextInput
*
*/
class cfd1DTextInput : public ves::xplorer::scenegraph::DCS
{
public:
    ///Constructor
    cfd1DTextInput( void );
    ///Destructor
    virtual ~cfd1DTextInput( void );
    ///get the Performer DCS (may need to remove)
    ves::xplorer::scenegraph::DCS* getpfDCS( void );
    ///set transform values.
    ///\param scale The scale value.
    ///\param trans The translation value.
    ///\param rot The rotation value.
    void SetTransforms( double scale[ 3 ] , double trans[ 3 ] , double rot[ 3 ] );
    ///Set the Filename of the dataset
    ///\param fileName
    void SetFilename( std::string fileName );
    ///update the text
    void Update( void );
    ///Update the colors
    ///\param red The red value.
    ///\param green The green value.
    ///\param blue The blue value.
    void UpdateTextColor( double red, double green, double blue );

private:

    std::string text;///<The string to hold the text.
    vtkActor*   actor;///<The VTKactor.
    //DCS*      dcs;
    osg::ref_ptr< ves::xplorer::scenegraph::Geode > geode;///<The geode.
};
}
}
#endif
