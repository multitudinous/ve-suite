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

#include <QtGui/QLineEdit>
#include <QtGui/QFileDialog>

#include <propertystore/ExternalStringSelect.h>

#include <ves/VEConfig.h>

/// @file FileEdit.h
/// @namespace ves::conductor
/// @class FileEdit is an editor widget consisting of a textedit and associated
/// button. When clicked, the button brings up a QFileDialog so the user may
/// select a file. Once chosen, the file path is converted to a relative path
/// (relative to the present working directory) and is placed into the textedit.
/// This class copies from propertystore::FileEdit and adds a small amount of
/// custom processing.
namespace ves
{
namespace conductor
{

class VE_CONDUCTOR_QTUI_EXPORTS FileEdit : public propertystore::ExternalStringSelect
{
    Q_OBJECT
public:
    FileEdit(QWidget *parent = 0);
    virtual propertystore::ExternalStringSelect* createNew( QWidget* parent );

public Q_SLOTS:
    virtual void buttonClicked();
    virtual void onFileSelected( const QString& filePath );
    virtual void onFileCancelled();

private:
    QFileDialog* m_fileDialog;
};

}} // ves::conductor
