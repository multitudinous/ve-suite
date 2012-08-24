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

#include <ves/xplorer/event/viz/VisFeatureMakerBase.h>

namespace ves
{
namespace conductor
{
/*!\file PolydataFeatureMaker.h
 * \class ves::conductor::PolydataFeatureMaker
 * \namespace ves::conductor
 *
 */
class PolydataFeatureMaker : public VisFeatureMakerBase
{
public:
    ///Constructor
    PolydataFeatureMaker();
    ///Copy constructor
    PolydataFeatureMaker( const PolydataFeatureMaker& orig );
    ///Destructor
    virtual ~PolydataFeatureMaker();
    ///Update method to generate vis feature
    virtual void Update( const::std::string& recordUUID );

protected:
    ///Called by the update function
    void AddPlane( propertystore::PropertySet& set );
    ///Called by AddPlane
    void UpdateContourInformation( propertystore::PropertySet& set );
    ///Setup the advanced properties
    virtual void UpdateAdvancedSettings( propertystore::PropertySet& set );

private:
    ///The countour setting data
    std::vector<ves::open::xml::DataValuePairPtr> m_contourInformation;

};

} // namespace conductor
} // namespace ves
