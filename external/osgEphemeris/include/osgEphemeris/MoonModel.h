/*
 -------------------------------------------------------------------------------
 | osgEphemeris - Copyright (C) 2007  Don Burns                                |
 |                                                                             |
 | This library is free software; you can redistribute it and/or modify        |
 | it under the terms of the GNU Lesser General Public License as published    |
 | by the Free Software Foundation; either version 3 of the License, or        |
 | (at your option) any later version.                                         |
 |                                                                             |
 | This library is distributed in the hope that it will be useful, but         |
 | WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY  |
 | or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public     |
 | License for more details.                                                   |
 |                                                                             |
 | You should have received a copy of the GNU Lesser General Public License    |
 | along with this software; if not, write to the Free Software Foundation,    |
 | Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.               |
 |                                                                             |
 -------------------------------------------------------------------------------
 */

#ifndef OSG_EPHEMERIS_MOON_MODEL_DEF
#define OSG_EPHEMERIS_MOON_MODEL_DEF

#include <string>
#include <osgEphemeris/Export.h>
#include <osgEphemeris/Sphere.h>

namespace osgEphemeris {

    /** \class MoonModel
        \brief The model of the Moon.
      */

class OSGEPHEMERIS_EXPORT MoonModel : public Sphere
{
    public:
        /**
          Default constructor
          */
        MoonModel();

        /**
          Return the size of the moon's radius
          */
        static double getMoonRadius() { return _moonRadius; }
        /**
          Set the position of the Sun with respect to the view.  This position
          is used to determine the lighting direction parameters, resulting in
          the correct representation of the phases of the moon.
          */
        void setSunPosition( osg::Vec3 sun );

    private:

        OSGEPHEMERIS_LOCAL static const double _moonRadius;
        OSGEPHEMERIS_LOCAL static std::string _vertexShaderProgram;
        OSGEPHEMERIS_LOCAL static std::string _fragmentShaderProgram;

        int _baseTextureUnit;
        int _bumpTextureUnit;

        osg::ref_ptr<osg::Uniform> _light;

        OSGEPHEMERIS_LOCAL static unsigned int  _moonImageLoLodWidth;
        OSGEPHEMERIS_LOCAL static unsigned int _moonImageLoLodHeight;
        OSGEPHEMERIS_LOCAL static unsigned int  _moonImageInternalTextureFormat;
        OSGEPHEMERIS_LOCAL static unsigned int _moonImagePixelFormat;
        OSGEPHEMERIS_LOCAL static unsigned char _moonImageLoLodData[];
        OSGEPHEMERIS_LOCAL static unsigned int  _moonImageHiLodWidth;
        OSGEPHEMERIS_LOCAL static unsigned int _moonImageHiLodHeight;
        OSGEPHEMERIS_LOCAL static unsigned char _moonImageHiLodData[];

        OSGEPHEMERIS_LOCAL static unsigned int  _moonNormalImageLoLodWidth;
        OSGEPHEMERIS_LOCAL static unsigned int _moonNormalImageLoLodHeight;
        OSGEPHEMERIS_LOCAL static unsigned int  _moonNormalImageInternalTextureFormat;
        OSGEPHEMERIS_LOCAL static unsigned int _moonNormalImagePixelFormat;
        OSGEPHEMERIS_LOCAL static unsigned char _moonNormalImageLoLodData[];
        OSGEPHEMERIS_LOCAL static unsigned int  _moonNormalImageHiLodWidth;
        OSGEPHEMERIS_LOCAL static unsigned int _moonNormalImageHiLodHeight;
        OSGEPHEMERIS_LOCAL static unsigned char _moonNormalImageHiLodData[];

        void _buildStateSet();
};

}

#endif
