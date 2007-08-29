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
#include "VE_Xplorer/SceneGraph/Utilities/PerlinNoiseTexture.h"
#include "VE_Xplorer/SceneGraph/Utilities/PerlinNoise.h"
#include <osg/Texture3D>
using namespace VE_SceneGraph::Utilities;
/////////////////////////////////////////////////////////////////////
PerlinNoiseTexture::PerlinNoiseTexture(unsigned int sdim,
                                                unsigned int tdim,
                                                unsigned int rdim)
{
    _initNoiseImage(sdim, tdim, rdim);
    _initNoiseTexture(sdim, tdim, rdim);
}
////////////////////////////////////////////////////////////////////
PerlinNoiseTexture::~PerlinNoiseTexture()
{
}
///////////////////////////////////////////////////////////////////////////
void PerlinNoiseTexture::_initNoiseImage(int s, int t, int r)
{
    ///\Note Need to add checks for texture type!!!
    m_noiseImage = new osg::Image();
    m_noiseImage->setImage(s, t, r,
                                    4, GL_RGBA, GL_UNSIGNED_BYTE,
                                   new unsigned char[4*s*t*r],
                                   osg::Image::USE_NEW_DELETE);

    const int startFrequency = 4;
    const int numOctaves = 4;

    int f, i, j, k, inc;
    double ni[3];
    double inci, incj, inck;
    int frequency = startFrequency;
    GLubyte *ptr;
    double amp = 0.5;

    for (f = 0, inc = 0; f < numOctaves; ++f, frequency *= 2, ++inc, amp *= 0.5)
    {
        VE_SceneGraph::Utilities::SetNoiseFrequency(frequency);
        ptr = m_noiseImage->data();
        ni[0] = ni[1] = ni[2] = 0;

        inci = 1.0 / (r / frequency);
        for (i = 0; i < r; ++i, ni[0] += inci)
        {
            incj = 1.0 / (t / frequency);
            for (j = 0; j < t; ++j, ni[1] += incj)
            {
                inck = 1.0 / (s / frequency);
                for (k = 0; k < s; ++k, ni[2] += inck, ptr += 4)
                {
                    *(ptr+inc) = (GLubyte) (((VE_SceneGraph::Utilities::noise3(ni) + 1.0) * amp) * 128.0);
                }
            }
        }
    }      
}
//////////////////////////////////////////////////////////////////////////
void PerlinNoiseTexture::_initNoiseTexture(int s, int t, int r )
{
    ///\Note Need to add checks for texture type!!!
    m_noiseTexture = new osg::Texture3D;
    m_noiseTexture->setFilter(osg::Texture3D::MIN_FILTER, osg::Texture3D::LINEAR);
    m_noiseTexture->setFilter(osg::Texture3D::MAG_FILTER, osg::Texture3D::LINEAR);
    m_noiseTexture->setWrap(osg::Texture3D::WRAP_S, osg::Texture3D::REPEAT);
    m_noiseTexture->setWrap(osg::Texture3D::WRAP_T, osg::Texture3D::REPEAT);
    m_noiseTexture->setWrap(osg::Texture3D::WRAP_R, osg::Texture3D::REPEAT);
    dynamic_cast<osg::Texture3D*>(m_noiseTexture.get())->setImage(  m_noiseImage.get() );
}
////////////////////////////////////////////////////////////////
osg::Texture* PerlinNoiseTexture::GetNoiseTexture()
{
    return m_noiseTexture.get();
}