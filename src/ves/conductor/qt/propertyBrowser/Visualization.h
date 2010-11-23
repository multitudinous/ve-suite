/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2010 by Iowa State University
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
#ifndef VISUALIZATION_H
#define VISUALIZATION_H

#define QT_NO_KEYWORDS

#include <QtGui/QDialog>

#include <ves/conductor/qt/ContourFeatureMaker.h>

#include <ves/xplorer/data/PropertySetPtr.h>

// Forward declarations
namespace Ui
{
class Visualization;
}

namespace ves
{
namespace conductor
{
class PropertyBrowser;

class Visualization : public QDialog
{
    Q_OBJECT
public:
    Visualization( QWidget* parent = 0 );
    ~Visualization();

protected:
    void changeEvent( QEvent* e );
    void UpdateFeatureIDSelectorChoices();

protected Q_SLOTS:
    // For info on Automatic connection of signals and slots, see
    // http://doc.trolltech.com/4.6/designer-using-a-ui-file.html#automatic-connections
    void on_WritePropertiesButton_clicked(); // Automatic connection
    void on_RefreshPropertiesButton_clicked(); // Automatic connection
    void on_NewFeatureButton_clicked(); // Automatic connection
    void on_DeleteFeatureButton_clicked(); // Automatic connection
    void on_FeaturesList_currentTextChanged( const QString& currentText ); // Automatic connection
    void on_FeatureIDSelector_currentIndexChanged ( const QString& text ); // Automatic connection


private:
    Ui::Visualization* m_ui;
    PropertyBrowser* mFeatureBrowser;

    ves::xplorer::data::PropertySetPtr mTempSet;
};

} // namespace conductor
} // namespace ves

#endif // VISUALIZATION_H
