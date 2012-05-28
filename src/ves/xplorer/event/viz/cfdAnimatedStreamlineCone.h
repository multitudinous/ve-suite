/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
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
#ifndef CFD_ANIMATED_STREAMLINE_CONE_H
#define CFD_ANIMATED_STREAMLINE_CONE_H

#include <ves/xplorer/event/viz/cfdObjects.h>

#include <vtkType.h>

class vtkPolyDataMapper;
class vtkPolyData;
class vtkGlyph3D;
class vtkSphereSource;


namespace ves
{
namespace xplorer
{
class cfdStreamers;
/*!\file cfdAnimatedStreamlineCone.h
 *   cfdAnimatedStreamlineCone API
 * \class ves::xplorer::cfdAnimatedStreamlineCone
 *
 */
class VE_XPLORER_EXPORTS cfdAnimatedStreamlineCone : public cfdObjects
{
public:
    ///Base constructor
    cfdAnimatedStreamlineCone( void );

    ///Copy consturctor
    cfdAnimatedStreamlineCone( cfdAnimatedStreamlineCone const& src );

    ///Destructor
    virtual ~cfdAnimatedStreamlineCone();

    void SetStreamlineSource( cfdStreamers* streamers );

    ///Set the Polydata Source
    ///\param polySource
    void SetPolyDataSource( vtkPolyData* polySource );

    ///in future, multi-threaded apps will make a copy of VjObs_i commandArray
    virtual void UpdateCommand();

    ///Update
    virtual void Update( void );

    ///Create a copy of this object
    virtual cfdObjects* CreateCopy();

private:
    ///Determine is a streamline is going backwards
    ///\return Return true if streamline is going backwards
    bool IsStreamlineBackwards( vtkIdType cellId );

    vtkPolyDataMapper* mapper;///<Mapper for vtk polydata
    vtkPolyData* polyData;///<polyData
    vtkGlyph3D* glyph;///<glyph
    vtkSphereSource* sphere;///<sphere source

    float particleDiameter;///<Diameter of particle
    enum STREAM_DIRECTION
    {
        FORWARD,
        BACKWARD,
        BOTH
    };

    STREAM_DIRECTION streamDir;///<Stream direction

    cfdStreamers* m_streamers;
    ///Forward and backward points for streamlines
    std::vector< std::pair< vtkIdType, vtkIdType > > m_streamlines;
};
}
}
#endif
