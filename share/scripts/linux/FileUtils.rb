#!/usr/bin/env ruby
require 'optparse'
require 'ostruct'

class ArgvOptionsParser
   def self.parse(args)
      options = OpenStruct.new
      options.classname = []
      options.filenames = []
      options.namespace = ""
      options.path = ""
      options.add_copyright = true

      opts = OptionParser.new do |opts|
         opts.banner = "Usage: FileUtils.rb [options]"
         opts.separator ""

         opts.on("-c", "--class C",
                 "Class to gen Ptr file for(multiple -c ok)") do |classname|
            options.classname << classname
         end

         opts.on("-p", "--path P",
                 "Path to include Classes ves/open/") do |path|
                     options.path = path
         end

         opts.on("-n", "--namespace NS",
                 "Namespace that Class belongs in (ves::open)") do |namespace|
                     options.namespace = namespace
         end
         
         opts.on("-u", "--update file(s)",
              "File to update copyright for") do |filename|
                    options.filenames << filename
         end

         opts.on_tail("-h", "--help", "Show this message") do
            puts opts
            exit
         end
      end

      opts.parse!
      options
   end
end

options = ArgvOptionsParser.parse(ARGV)
copyright = []
copyright << "/*************** <auto-copyright.rb BEGIN do not edit this line> **************\n"
copyright << " *\n"
copyright << " * VE-Suite is (C) Copyright 1998-2011 by Iowa State University\n"
copyright << " *\n"
copyright << " * Original Development Team:\n"
copyright << " *   - ISU's Thermal Systems Virtual Engineering Group,\n"
copyright << " *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden\n"
#copyright << " *   - Reaction Engineering International, www.reaction-eng.com\n"
copyright << " *\n"
copyright << " * This library is free software; you can redistribute it and/or\n"
copyright << " * modify it under the terms of the GNU Library General Public\n"
copyright << " * License as published by the Free Software Foundation; either\n"
copyright << " * version 2 of the License, or (at your option) any later version.\n"
copyright << " *\n"
copyright << " * This library is distributed in the hope that it will be useful,\n"
copyright << " * but WITHOUT ANY WARRANTY; without even the implied warranty of\n"
copyright << " * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU\n"
copyright << " * Library General Public License for more details.\n"
copyright << " *\n"
copyright << " * You should have received a copy of the GNU Library General Public\n"
copyright << " * License along with this library; if not, write to the\n"
copyright << " * Free Software Foundation, Inc., 59 Temple Place - Suite 330,\n"
copyright << " * Boston, MA 02111-1307, USA.\n"
copyright << " *\n"
copyright << " * -----------------------------------------------------------------\n"
copyright << " * Date modified: $Date$\n"
copyright << " * Version:       $Rev$\n"
copyright << " * Author:        $Author$\n"
copyright << " * Id:            $Id$\n"
copyright << " * -----------------------------------------------------------------\n"
copyright << " *\n"
copyright << " *************** <auto-copyright.rb END do not edit this line> ***************/\n"

options.filenames.each do |file_name|
  if File.file? file_name
    file = File.new(file_name)
    lines = file.readlines
    file.close

    changes = false
    lines.each do |line|
      changes = true if line.include? "<auto-copyright.rb BEGIN"
    end
    
    if not changes
      no_header = ""
      no_header += file_name.to_s
      no_header += " does not contain a copyright header"
      puts no_header
    end

    # Don't write the file if there are no changes
    copyright_changing = false
    if changes
      file = File.new(file_name,'w')
      lines.each do |line|
        if line.include? "<auto-copyright.rb BEGIN"
          copyright_changing = true
          file_changing = "Changing: "
          file_changing += file_name.to_s
          puts file_changing
          copyright.each do |cp_line|
            file.write(cp_line)
          end
        end
        if not copyright_changing
          file.write(line)
        end
        copyright_changing = false if line.include? "<auto-copyright.rb END"
      end
      file.close
    end
  end
end

namespace_array = options.namespace.split("::")
options.classname.each do |classname|
   if not File.exist? classname + "Ptr.h"
      puts classname + "Ptr.h"
      File.open(classname + "Ptr.h", 'w') do |pfile|
         if options.add_copyright
            copyright.each do |line|
               pfile.puts line
            end
         end

         pound_def = ""
         namespace_array.each do |ns|
            pound_def= pound_def + "_" + ns
         end
         pound_def += "_"
         pound_def+= classname
         pound_def+= "_ptr_h_"
         pound_def.upcase!
         pfile.puts "#ifndef #{pound_def}"
         pfile.puts "#define #{pound_def}"
         pfile.puts ""
         pfile.puts "#include <ves/util/PointerTypes.h>"
         pfile.puts ""
         pfile.puts "/**"
         pfile.puts " * \\file #{options.path}#{classname}Ptr.h"
         pfile.puts " *"
         pfile.puts " * Include this file to get a forward declaration of the type"
         pfile.puts " * #{options.namespace}::#{classname} and its pointer types."
         pfile.puts " * For the full declaration of #{options.namespace}::#{classname}"
         pfile.puts " * #{options.path}#{classname}.h must be included, too."
         pfile.puts " */"
         pfile.puts ""
         namespace_array.each do |ns|
            pfile.puts "namespace #{ns}"
            pfile.puts "{"
         end
         pfile.puts "class #{classname};"
         pfile.puts "/// Typedef for the SmartPtr types."
         pfile.puts "typedef ves::util::ClassPtrDef<#{classname}>::type  #{classname}Ptr;"
         pfile.puts "typedef ves::util::SharedPtrDef<#{classname}>::type #{classname}SharedPtr;"
         pfile.puts "typedef ves::util::WeakPtrDef<#{classname}>::type   #{classname}WeakPtr;"
         pfile.puts "typedef ves::util::ScopedPtrDef<#{classname}>::type #{classname}ScopedPtr;"
         namespace_array.each do
            pfile.puts "}"
         end
         pfile.puts "#endif"
      end
   end
end