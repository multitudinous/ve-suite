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
#pragma once

#include <ves/xplorer/event/viz/cfdObjects.h>

namespace ves
{
namespace xplorer
{
namespace event
{
namespace viz
{
/*!\file ParticleAnimation.h
 *  ParticleAnimation API
 * \class ves::xplorer::event::viz::ParticleAnimation
 *
 */
class VE_XPLORER_EXPORTS ParticleAnimation : public cfdObjects
{
public:
    ///Constructor.
    ParticleAnimation();

    ///Copy Constructor.
    ParticleAnimation( ParticleAnimation const& src );

    ///Destructor.
    virtual ~ParticleAnimation();

    ///Update.
    virtual void Update();

    ///Create a copy of this object
    virtual cfdObjects* CreateCopy();

    ///In future, multi-threaded apps will make a copy of VjObs_i commandArray.
    virtual void UpdateCommand();

    ///Assigns particle option.
    ///\param option
    void SetParticleOption( unsigned int option );

    ///Gets particle option.
    unsigned int GetParticleOption();

    ///Sets particle scale.
    ///\param x
    void SetParticleScale( float x );

    ///Gets particle scale.
    float GetParticleScale();

    void UpdatePropertySet();

private:
    ///Sphere scaling.
    float GetSphereScaleFactor();
    ///String to hold color by scalar.
    std::string colorByScalar;
    ///Test for warped surface.
    bool warpSurface;
    ///<warped contour scale value
    double warpedContourScale;

    ///point cloud or variably sized spheres.
    unsigned int _particleOption;
    ///particle scale.
    float _particleScale;
};
}
}
}
}
