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
#ifndef ConstraintPlugin_UIDialog_H
#define ConstraintPlugin_UIDialog_H

#include <QtGui/QWidget>
#include <QtGui/QComboBox>
#include <QtCore/QStringList>
#include <QtGui/QListWidgetItem>

#include <ves/util/SimpleDataTypeSignalSignatures.h>

#include <string>
#include <vector>
#include <map>

namespace Ui {
    class ConstraintPlugin_UIDialog;
}

class ConstraintPlugin_UIDialog : public QWidget
{
    Q_OBJECT

public:
    explicit ConstraintPlugin_UIDialog(QWidget *parent = 0);
    ~ConstraintPlugin_UIDialog();

protected:
    void changeEvent(QEvent *e);

protected slots:
    
    void on_m_sensorClientConnect_clicked();
    void on_m_heaterClientConnect_clicked();
    void on_m_testTableView_clicked();

private:

    Ui::ConstraintPlugin_UIDialog *ui;

    std::vector< std::string > mPartNumberList;
    ///PArt numbers loaded from the csv files
    std::vector< std::string > mLoadedPartNumbers;
    ///Description of part numbers loaded from csv files
    std::vector< std::string > mPartNumberDescriptions;
    //wxComboBox* mPartListCMB; // now lives in .ui file and is called m_partListCombo
    //wxArrayString m_partNumberStrings;
    QStringList m_partNumberStrings;
    //wxArrayString m_columnStrings;
    QStringList m_columnStrings;
    ///The number of tables created by the user
    size_t m_tableCounter;
    ///List of tables created by the user
    std::vector< std::string > m_tableList;
    std::string m_filename;
    
    ///The connect signal for sensors
    ves::util::TwoStringSignal_type m_connectSensorSignal;
    ///The connect signal for sensors
    ves::util::TwoStringSignal_type m_connectHeaterSignal;
};

#endif // ConstraintPlugin_UIDialog_H
