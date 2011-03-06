/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
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
#ifndef CFD_MOMENTUMS_H
#define CFD_MOMENTUMS_H


#include <ves/xplorer/event/viz/cfdContourBase.h>

class vtkWarpVector;

namespace ves
{
namespace xplorer
{
class cfdPlanes;
}
}


namespace ves
{
namespace xplorer
{
/*!\file cfdMomentums.h
 * cfdMomentums API
 * \class ves::xplorer::cfdMomentums
 *   A class that generates warped contour plots on multiple planes of data.
 *   VTK momentums renderer.
 */
class VE_XPLORER_EXPORTS cfdMomentums : public cfdContourBase
{
public:
    ///Initialize the multiple momentum profiles, based on the input
    ///from the vtkPolyData generated from cfdPlanes.
    ///\param xyz
    cfdMomentums( const int xyz );
    
    ///Copy constructor
    cfdMomentums( cfdMomentums const& src );

    ///Destructor
    virtual ~cfdMomentums( void );

    ///Output an updated pfGeoSet.
    virtual void Update( void );

    ///Create a copy of this object
    virtual cfdObjects* CreateCopy();

private:
    vtkWarpVector* warper;///<warper used by vtk
    cfdPlanes* planes;///<planes
};
}
}
#endif
