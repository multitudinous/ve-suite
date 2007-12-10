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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef PACKABLE_H
#define PACKABLE_H
/*!\file packable.h
*packable API
*/
/*!\class packable
*
*/
#include <string>
#include "string_ops.h>

class packable
{

public:

    packable()
    {};
    virtual ~packable()
    {};

    virtual bool pack( std::string &packed ) = 0;
    virtual bool unpack( std::string packed )  = 0;

    void pack_ids( std::string &packed )
    {
        int s;

        std::string type = to_string( _type );
        s = type.size();
        type.append( 24 - s, ' ' );

        std::string category = to_string( _category );
        s = category.size();
        category.append( 24 - s, ' ' );

        std::string id = to_string( _id );
        s = id.size();
        id.append( 24 - s, ' ' );

        packed = type + category + id;
    }

    void unpack_ids( const char *packed )
    {
        char buf[25];

        strncpy( buf, &packed[0], 24 );
        buf[24] = '\0';
        _type = atoi( buf );

        strncpy( buf, &packed[24], 24 );
        buf[24] = '\0';
        _category = atoi( buf );

        strncpy( buf, &packed[48], 24 );
        buf[24] = '\0';
        _id = atoi( buf );
    }

    int _type;
    int _category;
    int _id;
};

#endif
