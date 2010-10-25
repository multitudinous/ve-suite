/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2010 by Iowa State University
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
#ifndef VES_XPLORER_SCENEGRAPH_VTK_STREAMLINE_TEXTURE_CREATOR_H
#define VES_XPLORER_SCENEGRAPH_VTK_STREAMLINE_TEXTURE_CREATOR_H

#include <ves/xplorer/scenegraph/VectorFieldData.h>

#include <ves/VEConfig.h>

#include <osg/Vec3>

#include <vtkType.h>

#include <string>
#include <deque>

class vtkPolyData;

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
// Derived class for testing purposes. generates data at runtime.
class VE_SCENEGRAPH_EXPORTS VTKStreamlineTextureCreator : public VectorFieldData
{
public:
    VTKStreamlineTextureCreator();
    
    virtual osg::BoundingBox getBoundingBox();
    
    void SetPolyData( vtkPolyData* rawVTKData );
        
    void SetActiveVectorAndScalar( const std::string& vectorName, 
        const std::string& scalarName );

    osg::Image* CreateColorTextures( double* dataRange );

    struct Point
    {
        double x[ 3 ];
        vtkIdType vertId;
    };

    ///Set the list of points to use for a streamline
    void SetPointQueue( std::deque< Point >& pointList );

    ///Set the multiplier to use for interpolation purposes
    void SetPointMultiplier( unsigned int pointMultiplier );

protected:
    
    osg::Vec3 _sizes;
    vtkPolyData* m_rawVTKData;
    std::string m_vectorName;
    std::string m_scalarName;
    std::deque< Point > m_pointList;
    
    size_t m_pointMultiplier;
    
    virtual ~VTKStreamlineTextureCreator();
    
    virtual void internalLoad();
    
    void createDataArrays( float* pos, float* scalar );
    
};
}
}
}

#endif //VES_XPLORER_SCENEGRAPH_VTK_TEXTURE_CREATOR_H