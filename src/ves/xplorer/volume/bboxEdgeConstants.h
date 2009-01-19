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
 *************** <auto-copyright.rb END do not edit this line> ***************/
#ifndef EDGE_CONSTANTS_H
#define EDGE_CONSTANTS_H
///These should be in another class (brick manager)
unsigned int edgeIndex[24][2]  = {
                                     { 0, 1 },
                                     { 1, 4 },
                                     { 4, 7 },
                                     { 1, 5 },
                                     { 1, 5 },
                                     { 0, 1 },
                                     { 1, 4 },
                                     { 4, 7 },
                                     { 0, 2 },
                                     { 2, 5 },
                                     { 5, 7 },
                                     { 2, 6 },
                                     { 2, 6 },
                                     { 0, 2 },
                                     { 2, 5 },
                                     { 5, 7 },
                                     { 0, 3 },
                                     { 3, 6 },
                                     { 6, 7 },
                                     { 3, 4 },
                                     { 3, 4 },
                                     { 0, 3 },
                                     { 3, 6 },
                                     { 6, 7 }
                                 };
unsigned int edgeSequence[8][8] = {
                                      { 0, 1, 2, 4, 5, 3, 6, 7 },
                                      { 1, 3, 0, 5, 7, 2, 4, 6 },
                                      { 2, 0, 3, 6, 4, 1, 7, 5 },
                                      { 3, 2, 1, 7, 6, 0, 5, 4 },
                                      { 4, 5, 0, 6, 7, 1, 2, 3 },
                                      { 5, 7, 1, 4, 6, 3, 0, 2 },
                                      { 6, 4, 2, 7, 5, 0, 3, 1 },
                                      { 7, 6, 3, 5, 4, 2, 1, 0 }

                                  };

#endif
