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
#ifndef SUMMARY_VALUES_H
#define SUMMARY_VALUES_H

#include <stdio.h>
#include <string_ops.h>
#include "unit_conversion.h"

#include <vector>
#include <string>

class summary_values {

public:

  summary_values  ();
  ~summary_values ();

  void clear();

  void insert_summary_val (char *description, int value);
  void insert_summary_val (char *description, float value);
  void insert_summary_val (char *description, double value);
  void insert_summary_val (char *description, char *value);

  int size ();

  std::string get_description (int index);
  std::string get_value       (int index);
 
private:

  std::vector<std::pair<std::string, std::string> > summaries;

};

#endif
