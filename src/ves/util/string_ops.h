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
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef STRING_OPS_H
#define STRING_OPS_H

#include <string>
#include <vector>
#include "VE_Installer/include/VEConfig.h"

VE_CONDUCTOR_EXPORTS bool string_to_int    (const std::string &str, int &result);
VE_CONDUCTOR_EXPORTS bool string_to_double (const std::string &str, double &result);

VE_CONDUCTOR_EXPORTS std::string to_string (int val);
VE_CONDUCTOR_EXPORTS std::string to_string (unsigned int val);
VE_CONDUCTOR_EXPORTS std::string to_string (double val);

VE_CONDUCTOR_EXPORTS std::vector<std::string> split_string(const std::string& str, char sep);
#endif
