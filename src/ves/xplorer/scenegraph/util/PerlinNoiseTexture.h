/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
 * Date modified: $Date: 2007-07-24 16:05:18 -0500 (Tue, 24 Jul 2007) $
 * Version:       $Rev: 8482 $
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef PERLIN_NOISE_TEXTURE_H
#define PERLIN_NOISE_TEXTURE_H

#include <osg/Texture>
#include <osg/Image>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
namespace util
{
class PerlinNoiseTexture
{
public:
    ///Constructor
    ///\param xdim The texture size s dimension
    ///\param xdim The texture size t dimension
    ///\param xdim The texture size r dimension
    PerlinNoiseTexture(unsigned int sdim = 128,
                            unsigned int tdim = 128,
                            unsigned int rdim = 128);
    ///Destructor
    virtual ~PerlinNoiseTexture();

    ///Get the noise texture
    osg::Texture* GetNoiseTexture();
protected:
    ///Initialize the noise texture
   ///\param s The texture size s dimension
    ///\param t The texture size t dimension
    ///\param r The texture size r dimension
    void _initNoiseTexture(int s, int t, int r );

    ///Initialize the noise image data
    ///\param s The image size s dimension
    ///\param t The image size t dimension
    ///\param r The image size r dimension
    void _initNoiseImage(int s, int t, int r );

    osg::ref_ptr<osg::Texture> m_noiseTexture;///<The perlin noise texture.
    osg::ref_ptr<osg::Image> m_noiseImage;///<The perlin noise data.
};
}
}
}
}

#endif //PERLIN_NOISE_TEXTURE_H
