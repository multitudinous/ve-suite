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
#ifndef MODEL_CREATOR_H
#define MODEL_CREATOR_H

#include <ves/open/xml/CreationEventHandler.h>
#include <ves/open/xml/XMLObjectPtr.h>

namespace ves
{
namespace open
{
namespace xml
{
namespace model
{
/*!\file ModelCreator.h
  ModelCreator API
  */
/*!\class ves::open::xml::model::ModelCreator
 * Create Model Objects.
 */
class VE_MODEL_EXPORTS ModelCreator : public ves::open::xml::CreationEventHandler
{
public:
    ///Constructor
    ModelCreator()
    {
        ;
    }

    ///Destructor
    virtual ~ModelCreator()
    {
        ;
    }

    ///Create a new XMLObject.
    ///\param objectType The type of object to create.
    virtual ves::open::xml::XMLObjectPtr CreateNewXMLObject( const std::string& objectType );

    ///Create a new XMLObject.
    ///\param objectType The type of object to create.
    virtual ves::open::xml::XMLObjectPtr CreateNewXMLObjectSmart( const std::string& objectType );

    ///Create a copy of a new Model object
    ///\param objectType The type of object to create.
    ///\param objectToCopy The object to copy.
    virtual ves::open::xml::XMLObjectPtr CreateNewXMLObjectCopy( const std::string& objectType,
                                            const ves::open::xml::XMLObjectPtr& objectToCopy );

    ///Create a copy of a new Model object
    ///\param objectType The type of object to create.
    ///\param objectToCopy The object to copy.
    virtual ves::open::xml::XMLObjectPtr CreateNewXMLObjectCopySmart( const std::string& objectType,
                                                const ves::open::xml::XMLObjectPtr& objectToCopy );

protected:
};
}
}
}
}
#endif// MODEL_CREATOR_H


