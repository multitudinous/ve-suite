/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2004 by Iowa State University
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
 * File:          $RCSfile: ansysReader.h,v $
 * Date modified: $Date: 2004-05-18 16:09:54 -0500 (Tue, 18 May 2004) $
 * Version:       $Rev: 385 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef ANSYSREADER_H
#define ANSYSREADER_H

#include <iostream>

class ansysReader
{
   public:
      ansysReader( char * );
      ~ansysReader();

      void ReadHeader();
      void ReadSecondBlock();
      void ReadThirdBlock();
      void ReadFourthBlock();
      void ReadFifthBlock();
      void ReadSixthBlock();

   private:
      void FlipEndian();
      int ReadNthInteger( int n );

      char * ansysFileName;
      FILE *s1;
      bool endian_flip;
      int headerBlockSize;
      int secondBlockSize;
      int thirdBlockSize;
      int fourthBlockSize;
      int fifthBlockSize;
      int sixthBlockSize;

      int numNodes;
      int numElems;
};
#endif
