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
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/
#ifndef CFD_PBUFFER_QUAD_H
#define CFD_PBUFFER_QUAD_H
/*!\file cfdPBufferQuad.h
* cfdPBufferQuad API
*/

/*!\class ves::xplorer::volume::cfdPBufferQuad
*
*/
#ifdef _OSG
#include <osg/Drawable>
#include <osg/Version>

namespace osg
{
class BoundingBox;
class State;
class Texture3D;
}
#include <ves/VEConfig.h>
namespace ves
{
namespace xplorer
{
namespace volume
{
class VE_TEXTURE_BASED_EXPORTS cfdPBufferQuad : public osg::Drawable
{
public:
    cfdPBufferQuad();
    cfdPBufferQuad( const cfdPBufferQuad& pbQuad,
                    const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY );
    META_Object( PBufferQuad, cfdPBufferQuad );

    void SetBBox( float* bbox );
    void SetNumberOfSlices( unsigned int ns );
    void SetUseAutoTexCoords( bool useAutoTCoords = false );
    void SetTextureDimensions( unsigned int w, unsigned int h, unsigned int d );
    void CalculateSlices();

    void SetTextureToUpdate( osg::Texture3D* texture );
    //if advection doesn't work check this code!!!!!!!!
class BBoxCallback: public osg::Drawable::ComputeBoundingBoxCallback
    {
    public:
        BBoxCallback( cfdPBufferQuad* pbq )
        {
            _pbq = pbq;
        }
        virtual osg::BoundingBox computeBound( const osg::Drawable& ) const;
    protected:
        osg::ref_ptr<cfdPBufferQuad> _pbq;
    };
#if ((OSG_VERSION_MAJOR>=1) && (OSG_VERSION_MINOR>2) || (OSG_VERSION_MAJOR>=2))
    virtual void drawImplementation( osg::RenderInfo& temp ) const ;
#elif ((OSG_VERSION_MAJOR>=1) && (OSG_VERSION_MINOR<=2))
    virtual void drawImplementation( osg::State& state )const;
#endif
protected:
    void _drawAutoTexCoords()const;
    void _drawHardCodedTCoords( osg::State& state )const;
    virtual ~cfdPBufferQuad();
    bool _useAutoTexCoords;
    bool _bbSet;
    bool _sliceCountSet;

    unsigned int _nSlices;
    unsigned int _curSlice;
    unsigned int _w;
    unsigned int _h;
    unsigned int _d;
    float* _nearFarSlices;
    float* _slices;
    float _bounds[6];
    float _eye[3];
    float _lookAt[3];
    float _deltaZ;
    osg::ref_ptr<osg::Texture3D> _texture;
};
}
}
}
#endif// _OSG
#endif// CFD_PBUFFER_QUAD_H
