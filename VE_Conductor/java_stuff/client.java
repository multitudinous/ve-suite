
import javax.swing.*;
import javax.swing.event.*;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.lang.String;
import java.lang.Integer;
import java.lang.Math;
import java.util.Enumeration;
//import java.util.*;

public class client implements ActionListener
{
   public static my_orb clt = new my_orb();
   static Tabbed_But my_page = new Tabbed_But();
   public static draw_baf drawing_panel;
   public static send_param send_param_panel;
   static short datasetNum;
   static int activeDataSetType;
   static int numScalarsInActiveDataset;
   static int numVectorsInActiveDataset;
   static short numSounds;
   static short num_teacher;
   static short numSteadyStateDataSets;
   static boolean hasXPostData;
   static boolean hasYPostData;
   static boolean hasZPostData;
   static String[] scalarName;
   static String[] vectorName;
   static String[] datasetNames;
   static short [] datasetTypes;
   static short [] numScalarsPerDataset;
   static short [] numVectorsPerDataset;
   static String[] soundNameArray;
   static String[] teacher_attrib;
   static String[] dest;
   static short dest_num;
   static short dest_id;
   static Math my_math;
   static JCheckBox[] sound;
   static String sg1 = new String("X_");
   static String sg2 = new String("CONTOUR");
   static String sg3 = new String("S");
   static client listener = new client();
   static streamer_slider streamerSlider; //so tab changes reset cursor to none
   static id_mapper my_mapper;   //geode-related (visualization) commands
   static id_mapper my_mapper2;  //other commands
   static short [] sc_min;
   static short [] sc_max;
   static short timesteps;
   static TeacherPanel teacher;
   static ScalarPanel scalarPanel;
   static GeometryPanel geometryPanel;
   static DatasetList datasetList; 
   static TransientPanel transientPanel;
   static mainVizPanel vizPanel;
   static VectorPanel vectorPanel;
   static ParticlePanel particlePanel;
   static Iso_Panel isoPanel;
   //static JLabel[] cur_obj_name;  // song's disabled stuff

   public client()
   {
      dest_id = 0;
      try
      {
         System.out.println("reading in enumerated command lists");

         // the geode list will begin at index 0
         my_mapper = new id_mapper( "./config/mapping.config", 0 );

         // the non-geode command list begins at index 100
         my_mapper2 = new id_mapper( "./config/mapping2.config", 100 );
      }
      catch(IOException e)
      {
         System.out.println("IOException:"+e);
      }
   }

static class Tabbed_But extends JPanel 
{
   JScrollPane scroller;
   JScrollPane scroller2;
   cl my_cl;
   //JPanel status_panel;  //song's status stuff

   public Tabbed_But()
   {
      my_cl = new cl();
   }

   public void add_scene_obj(JLabel name)
   {
      //song's status stuff
      //this.status_panel.add(name);
   }

   public void clear_list_page()
   {
      //song's status stuff
      //this.status_panel.removeAll();
   }

   public void init()
   { 
      ImageIcon icon = new ImageIcon("middle.gif");
      JTabbedPane tabbedPane = new JTabbedPane();

      Component panel1 = makeButtonPanel(1);
      tabbedPane.addTab("Scalars     ", icon, panel1, "");
      tabbedPane.setSelectedIndex(0);

      Component panel2 = makeButtonPanel(2);
      tabbedPane.addTab("Design      ", icon, panel2, "");
      tabbedPane.setSelectedIndex(1);

      Component panel3 = makeButtonPanel(3);
      tabbedPane.addTab("Geometry    ", icon, panel3, "");
      tabbedPane.setSelectedIndex(2);

      Component panel0 = makeButtonPanel(0);
      tabbedPane.addTab("Visualization", icon, panel0, "");
      tabbedPane.setSelectedIndex(3);

      if ( timesteps == 0 )
      {
         Component panel4 = new JPanel(false);
         tabbedPane.addTab("Transient   ", icon, panel4, "");
         tabbedPane.setSelectedIndex(4);
         System.out.println("configuring tabbed page for case 4 -- but will disable");
         tabbedPane.setEnabledAt(4,false);
      }
      else
      {
         Component panel4 = makeButtonPanel(4);
         tabbedPane.addTab("Transient   ", icon, panel4, "");
         tabbedPane.setSelectedIndex(4);
      }

      Component panel5 = makeButtonPanel(5);
      tabbedPane.addTab("Teacher     ", icon, panel5, "");
      tabbedPane.setSelectedIndex(5);

      Component panel8 = makeButtonPanel(8);
      tabbedPane.addTab("Design Parameters", icon, panel8, "");
      //tabbedPane.setSelectedIndex(8);

      Component panel6 = makeButtonPanel(6);
      tabbedPane.addTab("Streamlines ", icon, panel6, "");
      tabbedPane.setSelectedIndex(6);

      Component panel7 = makeButtonPanel(7);
      tabbedPane.addTab("Vectors     ", icon, panel7, "");
      tabbedPane.setSelectedIndex(7);

      Component panel9 = makeButtonPanel(9);
      tabbedPane.addTab("Sounds      ", icon, panel9, "");
      tabbedPane.setSelectedIndex(9);

      Component panel10 = makeButtonPanel(10);
      tabbedPane.addTab("Vertex Data", icon, panel10, "");
      tabbedPane.setSelectedIndex(10);

      Component panel11 = makeButtonPanel(11);
      tabbedPane.addTab("View Locations", icon, panel11, "");
      tabbedPane.setSelectedIndex(11);

      Component panel12 = makeButtonPanel(12);
      tabbedPane.addTab("Datasets    ", icon, panel12, "");
      tabbedPane.setSelectedIndex(12);

      tabbedPane.addChangeListener(my_cl);

      //Add the tabbed pane to this panel.
      setLayout(new GridLayout(1,1,10,10)); 
      add(tabbedPane);

      tabbedPane.repaint();
   }

   class cl implements ChangeListener
   {
      public void stateChanged(ChangeEvent e)
      {
         JTabbedPane source = (JTabbedPane)e.getSource();
         //System.out.println("source.getSelectedIndex() = "+source.getSelectedIndex());
         if (source.getSelectedIndex() == 1)
         {
            // let any change in selected tab, set the cursor to none...
            int temp = streamerSlider.lastCursor;
            streamerSlider.cursorType[0].setSelected(true);
            streamerSlider.lastCursor = temp;
            drawing_panel.canv.setVisible(true);
         }
         else if (source.getSelectedIndex() != 7)  // all but streamline tab
         {
            // let any change in selected tab, set the cursor to none...
            int temp = streamerSlider.lastCursor;
            streamerSlider.cursorType[0].setSelected(true);
            streamerSlider.lastCursor = temp;
         }
         else //(source.getSelectedIndex() == 7)   // streamline tab
         {
            System.out.println("streamline tab");
            // when switch back to streamline tab, reset cursor to previous...
            //System.out.println("restoring lastCursor = "+streamerSlider.lastCursor);
            streamerSlider.cursorType[streamerSlider.lastCursor].setSelected(true);
         }
      }
   }

//throws IOException
protected Component makeButtonPanel(int type)
{
   JPanel panel = new JPanel(false);
   JLabel label;  // generic label
   
   System.out.println("configuring tabbed page for case "+type);
   switch ( type )
   {
   case 0 :
      panel.setBorder( BorderFactory.createEmptyBorder(5,5,5,5) );
      panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));

      vizPanel = new mainVizPanel();
      panel.add( vizPanel );

      isoPanel = new Iso_Panel();
      panel.add( isoPanel );

      JCheckBox blueMenuToggle = new JCheckBox("blue menu");
      blueMenuToggle.setActionCommand("BLUE_MENU_TOGGLE");
      blueMenuToggle.addActionListener(listener);
      blueMenuToggle.setSelected(true);

      // gui will turn off blue menu by default
      clt.obsref.id = my_mapper2.get_id("BLUE_MENU_TOGGLE");
      clt.obj_vis.send_id();
      blueMenuToggle.setSelected(false);

      JCheckBox scalarBarToggle = new JCheckBox("scalar bar");
      scalarBarToggle.setActionCommand("SCALAR_BAR_TOGGLE");
      scalarBarToggle.addActionListener(listener);
      scalarBarToggle.setSelected(true);
/*
      if ( numScalarsInActiveDataset == 0 )
      {
         scalarBarToggle.setSelected(false);
         scalarBarToggle.setEnabled(false);
      }
*/
      JButton refreshButton = new JButton("refresh datasets");
      refreshButton.setActionCommand("REFRESH_DATASETS");
      refreshButton.setSize(new Dimension(50,30));
      refreshButton.addActionListener(listener); 

      JButton clearButton = new JButton("clear all");
      clearButton.setActionCommand("CLEAR_ALL");
      clearButton.setSize(new Dimension(50,30));
      clearButton.addActionListener(listener); 

      JButton exitButton = new JButton("exit");
      //exitButton.setBackground(Color.red);
      exitButton.setForeground(Color.red);
      exitButton.setActionCommand("EXIT");
      exitButton.setSize(new Dimension(50,30));
      exitButton.addActionListener(listener); 

      Box buttonBox = Box.createHorizontalBox();
      buttonBox.add( exitButton );
      buttonBox.add( Box.createHorizontalGlue() );
      buttonBox.add( blueMenuToggle );
      buttonBox.add( Box.createHorizontalStrut(5) );
      buttonBox.add( scalarBarToggle );
      buttonBox.add( Box.createHorizontalStrut(5) );
      buttonBox.add( refreshButton );
      buttonBox.add( Box.createHorizontalStrut(5) );
      buttonBox.add( clearButton );

      panel.add(buttonBox);

      break;

   case 1:  // scalar parameters

      panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));

      scalarPanel = new ScalarPanel();

      panel.add( scalarPanel );

      break;

   case 2:
      drawing_panel = new draw_baf();
      drawing_panel.setSize(new Dimension(600, 390));

      JButton b32 = new JButton("Clear");
      b32.setActionCommand("clear_draw");
      b32.addActionListener(listener);

      JButton b33 = new JButton("Done");
      b33.setActionCommand("done_draw");
      b33.addActionListener(listener);

      JButton b34 = new JButton("Get Perf");
      b34.setActionCommand("get_perf");
      b34.addActionListener(listener);

      JButton b35 = new JButton("Send");
      b35.setActionCommand("SEND_DRAW");
      b35.addActionListener(listener);

      drawing_panel.button_panel.add(b32);
      drawing_panel.button_panel.add(b33);
      drawing_panel.button_panel.add(b34);
      drawing_panel.sender_button_panel.add(b35);

      JRadioButton[] rb;
      rb = new JRadioButton[dest_num];
      ButtonGroup dgroup = new ButtonGroup();

      for(int i = 0; i < dest_num; i++)
      {
         if(i ==0)
            rb[i] = new JRadioButton(dest[i],true);
         else
            rb[i] = new JRadioButton(dest[i],false);

         rb[i].setActionCommand("dest"+ i);
         rb[i].addActionListener(listener);
         dgroup.add(rb[i]);
      }

      for(int i = 0; i < dest_num; i++)
      {
         drawing_panel.choice_panel.add(rb[i]);
      }

      /*JScrollPane scroller3 = new JScrollPane(drawing_panel);
      scroller3.setPreferredSize(new Dimension(600, 380));
      scroller3.setMinimumSize(new Dimension(600, 380));*/

      panel.add(drawing_panel);
         
      break;

   case 3: //geometry
      panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));

      geometryPanel = new GeometryPanel();

      panel.add( geometryPanel );

      break;

   case 4 :      // transient

      panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));

      transientPanel = new TransientPanel();

      panel.add( transientPanel );

      break;

   case 5 :      // teacher

      panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));

      teacher = new TeacherPanel();

      panel.add( teacher );

      break;

   case 8:

      send_param_panel = new send_param();
      
      JButton b36 = new JButton("Update");
      b36.setActionCommand("UPDATE_SEND_PARAM");
      b36.addActionListener(listener);

      send_param_panel.add_but(b36);

      panel.add(send_param_panel);

      break;

   case 6 :      // streamlines

      streamerSlider = new streamer_slider();
      streamerSlider.setAlignmentX(Component.CENTER_ALIGNMENT);

      panel.setLayout(new BoxLayout(panel, BoxLayout.X_AXIS));
      panel.add(streamerSlider);

      break;

   case 7:  // vector parameters

      vectorPanel = new VectorPanel();

      panel.setLayout(new BoxLayout(panel, BoxLayout.X_AXIS));
      panel.add( vectorPanel );

      break;

   case 9: //sounds

      JPanel spanel = new JPanel();
      spanel.setBorder( BorderFactory.createEmptyBorder(5,5,5,5) );
      spanel.setLayout(new BoxLayout(spanel, BoxLayout.Y_AXIS));

      for(int i = 0; i < numSounds; i++)
      {
         sound[ i ]= new JCheckBox( soundNameArray[ i ] );
         sound[ i ].setSelected(false);
         //sound[ i ].addItemListener( listener );
         spanel.add( sound[ i ] );
      }

      scroller2 = new JScrollPane(spanel);
      scroller2.setAlignmentX(CENTER_ALIGNMENT);

      JButton soundUpdate = new JButton("Update");
      soundUpdate.setActionCommand("UPDATE_SOUNDS");
      soundUpdate.addActionListener(listener);  
      soundUpdate.setAlignmentX(CENTER_ALIGNMENT);

      panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
      panel.add(scroller2);
      panel.add(soundUpdate);

      break;

   case 10: // Droplets/Particles

      panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));

      particlePanel = new ParticlePanel();

      panel.add( particlePanel );

      break;

   case 11: // View Locations

      panel.setLayout(new BoxLayout(panel, BoxLayout.X_AXIS));

      ViewLocationsPanel viewLocationsPanel = new ViewLocationsPanel();

      panel.add( viewLocationsPanel );

      break;

    case 12:  // list of all datasets

      panel.setLayout(new BoxLayout(panel, BoxLayout.X_AXIS));

      datasetList = new DatasetList(); 

      panel.add( datasetList );

      break;
      
      }

      return panel;
   }
}

static class Iso_Panel extends JPanel
{
   private JSlider iso_slide;

   public Iso_Panel( )
   {
      setLayout(new BoxLayout(this,BoxLayout.X_AXIS));

      iso_slide = new JSlider();
      iso_slide.setOrientation(JSlider.HORIZONTAL);
      iso_slide.setMinimum(0);
      iso_slide.setMaximum(100);
      iso_slide.setValue(0);
      iso_slide.setMajorTickSpacing(10);
      iso_slide.setMinorTickSpacing(1);
      iso_slide.setPaintTicks(true);
      iso_slide.setPaintLabels(true);
      iso_slide.setSnapToTicks(true);

      JButton b26 = new JButton("Update");
      b26.setForeground(Color.blue);
      b26.setMnemonic(KeyEvent.VK_D);
      b26.setActionCommand("Update");
      b26.addActionListener(listener); 

      add(iso_slide);
      add(Box.createRigidArea(new Dimension(10,50)));
      add(b26);
   }

   public void Enable( )
   {
      iso_slide.setEnabled(true);
      iso_slide.setPaintTicks(true);
      iso_slide.setPaintLabels(true);
   }

   public void Disable( )
   {
      iso_slide.setEnabled(false);
      iso_slide.setPaintTicks(false);
      iso_slide.setPaintLabels(false);
   }
}

static class mainVizPanel extends JPanel 
                          implements ActionListener,ItemListener
{
   private JRadioButton[] cat;
   private JRadioButton[] fill;
   private ButtonGroup fillButtonGroup;
   private JCheckBox cycle;
   private JCheckBox nearest;
   private JRadioButton[] tp;
   private JRadioButton[] dir;
   private JPanel categoryPanel;
   private JPanel directionPanel;
   private JPanel typePanel;
   private JPanel fillPanel;
   private ButtonGroup categoryButtonGroup;
   private ButtonGroup direction_group;
   private ButtonGroup typeButtonGroup;
   private int lastType = 1;

   public mainVizPanel() 
   {
      setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));

      categoryPanel = new JPanel();
      categoryPanel.setBorder(BorderFactory.createCompoundBorder(
                   BorderFactory.createTitledBorder("Category"),
                   BorderFactory.createEmptyBorder(-5,5,5,5)));
      categoryPanel.setLayout(new GridLayout(3,3,0,-5)); //rows,cols,padding

      cat = new JRadioButton[6];

      cat[0] = new JRadioButton("Contour",false);
      cat[0].setActionCommand("cat_CONTOUR");

      cat[1] = new JRadioButton("Warped Contour",false);
      cat[1].setActionCommand("cat_MOMENTUM");

      cat[2] = new JRadioButton("Vector",false);
      cat[2].setActionCommand("cat_VECTOR");

      cat[3] = new JRadioButton("Isosurface",false);
      cat[3].setActionCommand("cat_ISO");

      cat[4] = new JRadioButton("PIV_Image",false);
      cat[4].setActionCommand("cat_IMAGE_EX");

      cat[5] = new JRadioButton("Polydata",false);
      cat[5].setActionCommand("cat_POLYDATA");

      categoryButtonGroup = new ButtonGroup();
      for( int i = 0; i < 6; i++)
      {
         cat[i].setFont(new Font("SansSerif", Font.BOLD, 10));
         cat[i].addActionListener(this);
         categoryButtonGroup.add(cat[i]);
         categoryPanel.add(cat[i]);
      }

      fillPanel = new JPanel();
      fillPanel.setBorder(BorderFactory.createCompoundBorder(
            BorderFactory.createTitledBorder("Contour Type"),
            BorderFactory.createEmptyBorder(-5,5,5,5)));//top,left,bottom,right
      fillPanel.setLayout(new GridLayout(0,1,0,-5)); //rows,cols,padding

      fill = new JRadioButton[3];

      fill[0] = new JRadioButton("Graduated",true);
      fill[0].setActionCommand("GRADUATED_FILL");

      fill[1] = new JRadioButton("Banded",false);
      fill[1].setActionCommand("BANDED_FILL");

      fill[2] = new JRadioButton("Lined",false);
      fill[2].setActionCommand("LINED_FILL");

      fillButtonGroup = new ButtonGroup();
      for( int i = 0; i < 3; i++ )
      {
         fill[i].setFont(new Font("SansSerif", Font.BOLD, 10));
         fill[i].addActionListener(this);
         fillButtonGroup.add( fill[i] );
         fillPanel.add( fill[i] );
      }

      Box topBox = Box.createHorizontalBox();
      topBox.add(categoryPanel);
      topBox.add(Box.createHorizontalStrut(5));
      topBox.add(fillPanel);

      directionPanel = new JPanel();
      directionPanel.setBorder(BorderFactory.createCompoundBorder(
                         BorderFactory.createTitledBorder("Direction"),
                         BorderFactory.createEmptyBorder(-5,5,5,5)));
      //one column and as many rows as necessary...
      directionPanel.setLayout(new GridLayout(0,1,0,-5)); //rows,cols,padding

      dir = new JRadioButton[4];
      
      dir[0] = new JRadioButton("X",true);
      dir[0].setActionCommand("dir_X_");

      dir[1] = new JRadioButton("Y",false);
      dir[1].setActionCommand("dir_Y_");

      dir[2] = new JRadioButton("Z",false);
      dir[2].setActionCommand("dir_Z_");

      dir[3] = new JRadioButton("By wand",false);
      dir[3].setActionCommand("dir_");

      direction_group = new ButtonGroup();
      for( int i = 0; i < 4; i++)
      {
         dir[i].setFont(new Font("SansSerif", Font.BOLD, 10));
         dir[i].addItemListener(this);
         direction_group.add(dir[i]);
         directionPanel.add(dir[i]);
      }

      typePanel = new JPanel();
      typePanel.setBorder(BorderFactory.createCompoundBorder(
                         BorderFactory.createTitledBorder("Type"),
                         BorderFactory.createEmptyBorder(-5,5,5,5)));
      //one column and as many rows as necessary...
      typePanel.setLayout(new GridLayout(0,1,0,-5)); //rows,cols,padding

      tp = new JRadioButton[2];

      tp[0] = new JRadioButton("All precomputed data",false);
      tp[0].setActionCommand("type_S");

      tp[1] = new JRadioButton("Specify a single plane",false);
      tp[1].setActionCommand("type_");

      typeButtonGroup = new ButtonGroup();
      for( int i = 0; i < 2; i++)
      {
         tp[i].setFont(new Font("SansSerif", Font.BOLD, 10));
         tp[i].addItemListener(this);
         typeButtonGroup.add(tp[i]);
      }

      cycle = new JCheckBox("Cycle precomputed surfaces");
      cycle.setFont(new Font("SansSerif", Font.BOLD, 10));
      cycle.addItemListener(this);

      Box box0 = Box.createHorizontalBox();
      box0.add(Box.createHorizontalStrut(25));
      box0.add(cycle);

      nearest = new JCheckBox("Use nearest precomputed plane");
      nearest.setFont(new Font("SansSerif", Font.BOLD, 10));
      nearest.setActionCommand("CHANGE_PRECALC_FLAG");
      nearest.addActionListener(this);

      Box box1 = Box.createHorizontalBox();
      box1.add(Box.createHorizontalStrut(25));
      box1.add(nearest);

      typePanel.add(tp[1]);
      typePanel.add(box1);
      typePanel.add(tp[0]);
      typePanel.add(box0);

      Box box2 = Box.createHorizontalBox();
      box2.add(directionPanel);
      box2.add(Box.createHorizontalStrut(5));
      box2.add(typePanel);

      add(topBox);
      add(box2);
   }

   public void forceCallback( ButtonGroup buttongroup )
   {
      //System.out.println("forceCallback");
      JRadioButton button = getSelection( direction_group );
      JRadioButton notbutton = getNotSelection( direction_group );
      notbutton.setSelected(true);
      button.setSelected(true);
      //button.doClick();
   }

   public void actionPerformed(ActionEvent e) 
   {
      //System.out.println(e.getActionCommand());

      // if user chooses to visualize contour, momentum or vector, 
      // then enable directions and single or multiplane options
      if ( e.getActionCommand().equals("cat_CONTOUR") ||
           e.getActionCommand().equals("cat_MOMENTUM") ||
           e.getActionCommand().equals("cat_VECTOR") )
      {
         enableContainer(directionPanel);
         enableContainer(typePanel);
         forceCallback( direction_group );
         isoPanel.Enable();

         if ( e.getActionCommand().equals("cat_CONTOUR") ||
              e.getActionCommand().equals("cat_MOMENTUM") )
         {
            enableContainer(fillPanel);
         }
         else
         {
            disableContainer(fillPanel);
         }
      }
      else if ( e.getActionCommand().equals("cat_ISO") ||
                e.getActionCommand().equals("cat_IMAGE_EX") ||
                e.getActionCommand().equals("cat_POLYDATA") )
      {
         disableContainer(directionPanel);

         disableContainer(typePanel);
         nearest.setSelected(false);  // this must be off
         cycle.setSelected(false);

         if ( e.getActionCommand().equals("cat_ISO") )
            isoPanel.Enable();
         else
            isoPanel.Disable();

         disableContainer(fillPanel);

      }
      else if ( e.getActionCommand().equals("GRADUATED_FILL") ||
                e.getActionCommand().equals("BANDED_FILL") ||
                e.getActionCommand().equals("LINED_FILL") )
      {
         clt.obsref.id = my_mapper2.get_id("CHANGE_CONTOUR_FILL");

         if ( e.getActionCommand().equals("GRADUATED_FILL") )
            clt.obsref.iso_value = 0;
         else if ( e.getActionCommand().equals("BANDED_FILL") )
            clt.obsref.iso_value = 1;
         else if ( e.getActionCommand().equals("LINED_FILL") )
            clt.obsref.iso_value = 2;

         clt.obj_vis.send_id();
      }
      else if ( e.getActionCommand().equals("CHANGE_PRECALC_FLAG") )
      {
         clt.obsref.pre_state = 0;
         if ( nearest.isSelected() )
            clt.obsref.pre_state = 1;
         System.out.println(e.getActionCommand()+" "+clt.obsref.pre_state);
      }
   }

   public void itemStateChanged(ItemEvent e) 
   {
      Object source = e.getItemSelectable();

      if (e.getStateChange() == ItemEvent.SELECTED)
      {
         // if direction X, Y, or Z was selected, then
         // update multiplane options if precalculated plane data is available
         if ( source == dir[0] || source == dir[1] || source == dir[2] )
         {
            //System.out.println("click on a dir");
            if ( ( dir[0].isSelected() && hasXPostData ) ||
                 ( dir[1].isSelected() && hasYPostData ) ||
                 ( dir[2].isSelected() && hasZPostData ) )
            {
               //System.out.println("precalculated plane data is available");
               // when precalculated plane data is available,
               // enable "use precomputed planes", but default to single plane
               // with the "use precomputed data" option checked
               tp[0].setEnabled(true);
               tp[1].setEnabled(true);
               //System.out.println("lastType = "+lastType);
               if ( lastType == 0 )
                  tp[0].setSelected(true);
               else
               {
                  tp[1].setSelected(true);

                  // enable nearest single plane selection
                  nearest.setEnabled(true);
                  nearest.setSelected(true);
               }
            }
            else 
            {
               //System.out.println("precalculated plane data not available");
               // becuse precalculated plane data is not available,
               // only allow user to specify the plane position with slider and 
               // uncheck and disable CheckBox for nearest precomputed plane
               tp[0].setSelected(false);
               tp[0].setEnabled(false);

               // enable and click on single plane selection
               tp[1].setEnabled(true);
               tp[1].setSelected(true);

               // disable nearest single plane selection
               nearest.setSelected(false);
               nearest.setEnabled(false);
            }
            cycle.setEnabled(false);   // TODO: implemement this in cfdApp
         }
         else if ( source == dir[3] )
         {
            //System.out.println("dir[3].isSelected()");
            nearest.setSelected(false);
            disableContainer( typePanel );
            isoPanel.Disable();
         }

         //if user chooses to specify all planes, 
         //then uncheck and disable CheckBox for nearest precomputed plane
         else if(source == tp[0])
         {
            //System.out.println("click on tp[0]");
            isoPanel.Disable();

            nearest.setSelected(false);
            nearest.setEnabled(false);

            cycle.setSelected(false);
            //cycle.setEnabled(true); // TODO: implemement this in cfdApp

            lastType = 0;
            //System.out.println("lastType = "+lastType);
         }

         // if user chooses to specify a single plane, 
         // --AND if user has appropriate precalculated plane data available--
         // then check and enable CheckBox for nearest precomputed plane
         else if(source == tp[1])
         {
            //System.out.println("click on tp[1]");
            isoPanel.Enable();

            if ( ( dir[0].isSelected() && hasXPostData ) ||
                 ( dir[1].isSelected() && hasYPostData ) ||
                 ( dir[2].isSelected() && hasZPostData ) )
            {
               nearest.setEnabled(true);
               nearest.setSelected(true);
            }
            else
            {
               nearest.setEnabled(false);
               nearest.setSelected(false);
            }
            cycle.setEnabled(false);
            cycle.setSelected(false);

            lastType = 1;
            //System.out.println("lastType = "+lastType);
         }
      }
   }
}

/* 
   // song's status stuff
   status_panel = new JPanel();  // song's status stuff, global to Tabbed_but
   status_panel.setLayout(new BoxLayout(status_panel, BoxLayout.Y_AXIS));
   status_panel.setBorder(BorderFactory.createCompoundBorder(
                      BorderFactory.createTitledBorder("objects in VE"),
                      BorderFactory.createEmptyBorder(5,5,5,5)));
   status_panel.setPreferredSize(new Dimension(130, 520));
   //status_panel.setPreferredSize(new Dimension(120, 320));

   JScrollPane statusScroller = new JScrollPane(status_panel);
   statusScroller.setPreferredSize(new Dimension(150, 300));
   statusScroller.setMinimumSize(new Dimension(150, 300));
   //panel.add(statusScroller,BorderLayout.EAST);
*/

static class Double_slider extends JPanel 
                           implements ChangeListener 
{
   private String myCommand;
   private JSlider min_slide;
   private JSlider max_slide;

   public Double_slider( String title, String command ) 
   {
      myCommand = command;

      setBorder(BorderFactory.createCompoundBorder(
                      BorderFactory.createEtchedBorder(),
                      BorderFactory.createEmptyBorder(5,5,5,5)));
      setPreferredSize(new Dimension(170, 4000));
      setMaximumSize(new Dimension(200, 4000));
      setLayout(new BoxLayout(this,BoxLayout.Y_AXIS));

      JLabel label = new JLabel( title );
      label.setAlignmentX(Component.CENTER_ALIGNMENT);
      label.setFont(new Font("SansSerif", Font.BOLD, 14));

      add(label);

      JLabel min_label = new JLabel("Min(%)");
      min_label.setAlignmentX(Component.CENTER_ALIGNMENT);
      min_label.setFont(new Font("SansSerif", Font.BOLD, 10));

      JLabel max_label = new JLabel("Max(%) ");
      max_label.setAlignmentX(Component.CENTER_ALIGNMENT);
      max_label.setFont(new Font("SansSerif", Font.BOLD, 10));

      Box labelBox = Box.createHorizontalBox();
      labelBox.add(min_label);
      labelBox.add( Box.createHorizontalGlue() );
      labelBox.add(max_label);

      add(labelBox);

      min_slide = new JSlider();
      min_slide.setOrientation(JSlider.VERTICAL);
      min_slide.setMinimum(0);
      min_slide.setMaximum(100);
      min_slide.setValue(0);
      min_slide.addChangeListener(this);
      min_slide.setMajorTickSpacing(10);
      min_slide.setMinorTickSpacing(1);
      min_slide.setPaintTicks(true);
      min_slide.setPaintLabels(true);

      max_slide = new JSlider();
      max_slide.setOrientation(JSlider.VERTICAL);
      max_slide.setMinimum(0);
      max_slide.setMaximum(100);
      max_slide.setValue(100);
      max_slide.addChangeListener(this);
      max_slide.setMajorTickSpacing(10);
      max_slide.setMinorTickSpacing(1);
      max_slide.setPaintTicks(true);
      max_slide.setPaintLabels(true);

      Box sliderBox = Box.createHorizontalBox();
      sliderBox.add(min_slide);
      sliderBox.add( Box.createHorizontalGlue() );
      sliderBox.add(max_slide);

      add(sliderBox);
   }

   // Listen to the double sliders. 
   public void stateChanged(ChangeEvent e)
   {
      if ( numScalarsInActiveDataset == 0 )
         return;

      JSlider source = (JSlider)e.getSource();
      if (!source.getValueIsAdjusting()) 
      {
         int fps = (int)source.getValue();
         //System.out.print("fps = "+fps+"\n");
         if(source == min_slide)
         {
            //System.out.print("clt.obsref.sc = "+clt.obsref.sc+"\n");
            System.out.println("minimum changed to "+fps);
            clt.obsref.min = min_slide.getValue();
            clt.obsref.max = max_slide.getValue();

            if ( myCommand == "CHANGE_SCALAR_RANGE" )
            {
               //System.out.println("updated sc_min");
               sc_min[clt.obsref.sc] = (short)min_slide.getValue();
            }
            clt.obsref.id = my_mapper2.get_id( myCommand );
            clt.obj_vis.send_id();
         }
         else if(source == max_slide)
         {
            System.out.println("maximum changed to "+fps);
            clt.obsref.min = min_slide.getValue();
            clt.obsref.max = max_slide.getValue();

            if ( myCommand == "CHANGE_SCALAR_RANGE" )
            {
               //System.out.println("updated sc_max");
               sc_max[clt.obsref.sc] = (short)max_slide.getValue();
            }
            clt.obsref.id = my_mapper2.get_id( myCommand );
            clt.obj_vis.send_id();
         }
      }
   }
}

static class Single_slider extends JPanel 
                           implements ChangeListener 
{
   private String myCommand;
   private JSlider slide;

   public Single_slider( String title, int fontSize, String command,
                         int min, int max, int value, int showLabels ) 
   {
      myCommand = command;

      setBorder(BorderFactory.createCompoundBorder(
                      BorderFactory.createEtchedBorder(),
                      BorderFactory.createEmptyBorder(5,5,5,5)));
      setPreferredSize(new Dimension(140, 390));
      setLayout(new BoxLayout(this,BoxLayout.Y_AXIS));

      JLabel label = new JLabel( title );
      label.setAlignmentX(Component.CENTER_ALIGNMENT);
      label.setFont(new Font("SansSerif", Font.BOLD, fontSize));

      add(label);

      slide = new JSlider();
      slide.setOrientation(JSlider.VERTICAL);
      slide.setMinimum(min);
      slide.setMaximum(max);
      slide.setValue(value);
      slide.addChangeListener(this);
      slide.setMajorTickSpacing(max/10);
      slide.setMinorTickSpacing(1);
      slide.setSnapToTicks(true);
      if ( showLabels == 0 )
      {
         slide.setPaintLabels(false);
         slide.setPaintTicks(false);
      }
      else
      {
         slide.setPaintLabels(true);
         slide.setPaintTicks(true);
      }

      add(slide);
   }

   // Listen to the slider. 
   public void stateChanged(ChangeEvent e)
   {
      JSlider source = (JSlider)e.getSource();
      if (!source.getValueIsAdjusting()) 
      {
         if(source == slide)
         {
            int fps = (int)source.getValue();
            System.out.println("Slider changed to "+fps);
            clt.obsref.iso_value = fps;
            clt.obsref.id = my_mapper2.get_id( myCommand );
            clt.obj_vis.send_id();
         }
      }
   }
}

static class DatasetAndScalar extends JPanel 
                              implements ActionListener 
{
   private JPanel ds_panel;
   private JPanel scalar_panel;
   private JRadioButton [] dsButton;
   private int myDatasetType;
   private int numScalars;
   private int numVectors;
   private int numDatasetsOfType;
   private int [] lastScalarIndex;
   private int [] lastVectorIndex;
   private int [] datasetIndex;
   private int selectedDataSetOfGroup;
   private int lastSelectedDataSetOfGroup;
   private JRadioButton[] scalarButton;
   private int scalarStartIndex;
   private int vectorStartIndex;
   private ButtonGroup dsGroup;
   private int activeDataSetIndex = 0;
   private String myTitle;

   public DatasetAndScalar( String title, int datasetType ) 
   {
      myDatasetType = datasetType;
      myTitle = title;

      setBorder(BorderFactory.createCompoundBorder(
                      BorderFactory.createEtchedBorder(),
                      BorderFactory.createEmptyBorder(5,5,5,5)));
      setLayout(new BoxLayout(this,BoxLayout.Y_AXIS));

      ds_panel = new JPanel();
      ds_panel.setBorder( BorderFactory.createEmptyBorder(5,5,5,5) );
      ds_panel.setLayout(new BoxLayout(ds_panel, BoxLayout.Y_AXIS));

      JScrollPane datasetScroller = new JScrollPane(ds_panel);
      datasetScroller.setPreferredSize(new Dimension(300, 300));

      JLabel label = new JLabel( myTitle );
      label.setAlignmentX(Component.CENTER_ALIGNMENT);
      label.setFont(new Font("SansSerif", Font.BOLD, 14));

      add(label);
      add(datasetScroller);
      add(Box.createRigidArea(new Dimension(0,5)));

      JLabel label2 = new JLabel( "Scalars");
      label2.setAlignmentX(Component.CENTER_ALIGNMENT);
      label2.setFont(new Font("SansSerif", Font.BOLD, 14));
      add(label2);

      scalar_panel = new JPanel();
      scalar_panel.setBorder( BorderFactory.createEmptyBorder(5,5,5,5) );
      scalar_panel.setLayout(new BoxLayout(scalar_panel, BoxLayout.Y_AXIS));

      JScrollPane scalarScroller = new JScrollPane(scalar_panel);
      scalarScroller.setPreferredSize(new Dimension(300, 300));
      add(scalarScroller);
   }

   public void Refresh( ) 
   {
      // loop over all datasets and count how many are of a particular type
      numDatasetsOfType = 0;
      for( int i = 0; i < datasetNum; i++)
      {
         if (datasetTypes[ i ] == myDatasetType)
            numDatasetsOfType++;
      }

      System.out.println("for datasetType = " + myDatasetType + 
                           ", numDatasets = "+numDatasetsOfType);

      datasetIndex = new int [ numDatasetsOfType ];
      lastScalarIndex = new int [ numDatasetsOfType ];
      lastVectorIndex = new int [ numDatasetsOfType ];
      for( int i = 0; i < numDatasetsOfType; i++ )
      {
         lastScalarIndex[ i ] = -1;
         lastVectorIndex[ i ] = -1;
      }

      lastSelectedDataSetOfGroup = selectedDataSetOfGroup = 0;

      if (numDatasetsOfType > 0 )
         dsButton = new JRadioButton[ numDatasetsOfType ];

      // match up the dataset buttons for this dataset type to global datset numbering
      int ii = 0;
      for( int i = 0; i < datasetNum; i++)
      {
         if (datasetTypes[ i ] != myDatasetType)
            continue;

         dsButton[ ii ] = new JRadioButton(datasetNames[ i ],false);
         dsButton[ ii ].setActionCommand("dataset"+i);
         dsButton[ ii ].addActionListener(this);
         datasetIndex[ ii ] = i;
         ii++;
      }

      ds_panel.removeAll();

      if ( numDatasetsOfType > 0 )
      {
         dsGroup = new ButtonGroup();

         for( int i = 0; i < numDatasetsOfType; i++)
         {
            dsGroup.add(dsButton[ i ]);
            ds_panel.add(dsButton[ i ]);
         }
      }
      ds_panel.invalidate();
      ds_panel.validate();
      ds_panel.repaint();

      // set default dataset and scalar to first ones available
      if ( numDatasetsOfType > 0 )
      {
         lastScalarIndex[ 0 ] = 0;
         lastVectorIndex[ 0 ] = 0;
         dsButton[ 0 ].doClick();
      }
   }

   // Listen to the buttons. 
   public void actionPerformed(ActionEvent e) 
   {
      for(int i = 0; i < datasetNum; i++) 
      {
         if (e.getActionCommand().equals("dataset"+i) )
         {
            activeDataSetType = myDatasetType;
            activeDataSetIndex = i;

            System.out.println("CHANGE_STEADYSTATE_DATASET "+i);
            clt.obsref.id = my_mapper2.get_id("CHANGE_STEADYSTATE_DATASET");
            clt.obsref.iso_value = i;

            numScalars = numScalarsPerDataset[ i ];
//System.out.println("numScalars = "+numScalars);

            numVectors = numVectorsPerDataset[ i ];
//System.out.println("numVectors = "+numVectors);

            // which button in the group is selected?
            for( int ii = 0; ii < numDatasetsOfType; ii++)
            {
               if ( dsButton[ ii ].isSelected() )
               {
                  lastSelectedDataSetOfGroup = selectedDataSetOfGroup;
                  selectedDataSetOfGroup = ii;
                  break;
               }
            }

            enableContainer( scalar_panel );

//System.out.println("selectedDataSetOfGroup = "+selectedDataSetOfGroup);
//System.out.println("lastSelectedDataSetOfGroup = "+lastSelectedDataSetOfGroup);
//System.out.println("lastScalarIndex[selectedDataSetOfGroup] = "+lastScalarIndex[selectedDataSetOfGroup]);
            int index;
            if ( lastScalarIndex[ selectedDataSetOfGroup ] == -1 )
            {
               index = lastScalarIndex[ lastSelectedDataSetOfGroup ];
//System.out.println("index = "+index);

               if ( index >= numScalars )
                  index = 0;

               lastScalarIndex[ selectedDataSetOfGroup ] = index;
            }
            else
            {
               index = lastScalarIndex[ selectedDataSetOfGroup ];
            }

            clt.obsref.sc = index;
//System.out.println("clt.obsref.sc = "+clt.obsref.sc);

            clt.obsref.min = scalarPanel.scalarRangeSlider.min_slide.getValue();
            clt.obsref.max = scalarPanel.scalarRangeSlider.max_slide.getValue();
            clt.obj_vis.send_id();

            int vIndex;
            if ( lastVectorIndex[ selectedDataSetOfGroup ] == -1 )
            {
               vIndex = lastVectorIndex[ lastSelectedDataSetOfGroup ];
//System.out.println("vIndex = "+vIndex);

               if ( vIndex >= numVectors )
                  vIndex = 0;

               lastVectorIndex[ selectedDataSetOfGroup ] = vIndex;
            }
            else
            {
               vIndex = lastVectorIndex[ selectedDataSetOfGroup ];
            }

            scalar_panel.removeAll();

            // this is global to talk with scalars/vectors page
            numScalarsInActiveDataset = numScalars;
            numVectorsInActiveDataset = numVectors;

            // loop over all datasets and get the scalar starting index 
            // for this particular dataset
            scalarStartIndex = 0;
            vectorStartIndex = 0;
            for( int ii = 0; ii < datasetNum; ii++)
            {
               if ( ii == i )
                  break;
               else
               {
                  scalarStartIndex += numScalarsPerDataset[ ii ];
                  vectorStartIndex += numVectorsPerDataset[ ii ];
               }
            }

            scalarButton = new JRadioButton[ numScalars ];
            ButtonGroup scalarGroup = new ButtonGroup();

            for( int ii = 0; ii < numScalars; ii++)
            {
//System.out.println("    new scalar name = "+scalarName[scalarStartIndex+ii]);
               scalarButton[ii] = new JRadioButton(
                                      scalarName[scalarStartIndex+ii], false);
               scalarButton[ii].setActionCommand("scalar"+ ii);
               scalarButton[ii].addActionListener(this);
               scalarButton[ii].setEnabled(true);

               scalarGroup.add(scalarButton[ii]);
               scalar_panel.add(scalarButton[ii]);
               scalarButton[ii].repaint();
            }

            scalar_panel.invalidate();
            scalar_panel.validate();
            scalar_panel.repaint();

            if ( numScalars > 0 )
            {
//System.out.println("setting scalar button to value: "+index);
               scalarButton[ index ].setSelected(true);
            }

            updateScalarTabScalarPanel( index );
            updateVectorTabVectorPanel( vIndex );

            break;
         }
      }

      for(int i = 0; i < numScalars; i++) 
      {
         if (e.getActionCommand().equals("scalar"+i) )
         {
            activeDataSetType = myDatasetType;

            System.out.println("scalar button "+i+" selected");
            //System.out.println("selectedDataSetOfGroup = "+selectedDataSetOfGroup);
            lastScalarIndex[selectedDataSetOfGroup] = i;
            scalarPanel.scalarRangeSlider.min_slide.setValue(sc_min[ i ]);
            scalarPanel.scalarRangeSlider.max_slide.setValue(sc_max[ i ]);
            clt.obsref.id = my_mapper2.get_id("CHANGE_STEADYSTATE_DATASET");
            clt.obsref.iso_value = datasetIndex[selectedDataSetOfGroup];
            clt.obsref.sc = i;         // using zero-based scalar counting
            clt.obsref.min = scalarPanel.scalarRangeSlider.min_slide.getValue();
            clt.obsref.max = scalarPanel.scalarRangeSlider.max_slide.getValue();
            System.out.println("CHANGE_STEADYSTATE_DATASET "+datasetIndex[selectedDataSetOfGroup]+", scalar "+i);
            clt.obj_vis.send_id();

            updateScalarTabScalarPanel( i );

            break;
         }
      }
   }

   private void updateScalarTabScalarPanel( int i ) 
   {
      scalarPanel.SCALAR_PANEL.removeAll();

      // Create a label for the group
      JLabel label = new JLabel("Scalars");
      label.setFont(new Font("SansSerif", Font.BOLD, 14));
      scalarPanel.SCALAR_PANEL.add( label );

//System.out.println("numScalars = "+numScalars);
      if ( numScalars > 0 )
      {
         scalarPanel.r = new JRadioButton[ numScalars ];
     
         // add buttons to buttongroup and add buttons to panel..
         ButtonGroup mgroup = new ButtonGroup();
         for( int ii = 0; ii < numScalars; ii++)
         {
            scalarPanel.r[ ii ] = new JRadioButton(
                                      scalarName[scalarStartIndex+ii], false);
            scalarPanel.r[ ii ].setActionCommand("scalar"+ii);
            scalarPanel.r[ ii ].addActionListener(scalarPanel);

            mgroup.add( scalarPanel.r[ ii ] );
            scalarPanel.SCALAR_PANEL.add( scalarPanel.r[ ii ] );
         }
         scalarPanel.r[ i ].setSelected(true);
      }
      else
         scalarPanel.r = null;

      scalarPanel.SCALAR_PANEL.invalidate();
      scalarPanel.SCALAR_PANEL.validate();
      scalarPanel.SCALAR_PANEL.repaint();

   }

   private void updateVectorTabVectorPanel( int i ) 
   {
      vectorPanel.VECTOR_PANEL.removeAll();

      // Create a label for the group
      JLabel label = new JLabel("Vectors");
      label.setFont(new Font("SansSerif", Font.BOLD, 14));
      vectorPanel.VECTOR_PANEL.add( label );

//System.out.println("numVectors = "+numVectors);
      if ( numVectors > 0 )
      {
         vectorPanel.vecButton = new JRadioButton[ numVectors ];
         // add buttons to buttongroup and add buttons to panel..
         ButtonGroup mgroup = new ButtonGroup();
         for( int ii = 0; ii < numVectors; ii++)
         {
//System.out.println("vectorStartIndex = "+vectorStartIndex+", ii = "+ii+", vectorName = "+vectorName[vectorStartIndex+ii]);
            vectorPanel.vecButton[ ii ] = new JRadioButton(
                                         vectorName[vectorStartIndex+ii], false);
            vectorPanel.vecButton[ ii ].setActionCommand("vector"+ii);
            vectorPanel.vecButton[ ii ].addActionListener(vectorPanel);

            mgroup.add( vectorPanel.vecButton[ ii ] );
            vectorPanel.VECTOR_PANEL.add( vectorPanel.vecButton[ ii ] );
         }
         vectorPanel.vecButton[ i ].setSelected(true);
      }
      else
         vectorPanel.vecButton = null;

      vectorPanel.VECTOR_PANEL.invalidate();
      vectorPanel.VECTOR_PANEL.validate();
      vectorPanel.VECTOR_PANEL.repaint();
   }
}

static class streamer_slider extends JPanel 
                             implements ChangeListener,
                                        ItemListener,
                                        ActionListener
{
   private JSlider streamer1;
   private JSlider streamer2;
   private JButton steamlineLoopButton;
   private JButton steamlineComputeButton;
   private JButton particleTrackButton;
   private JCheckBox seedpointToggle;
   private JPanel streamerSliderPanel;
   private JRadioButton [] direction;
   private ButtonGroup directionButtonGroup;
   private JPanel directionPanel;
   private JRadioButton [] cursorType;
   private JRadioButton [] integrationType;
   private ButtonGroup cursorSelectionButtonGroup;
   private ButtonGroup integrationPanelButtonGroup;
   private JPanel cursorSelectionPanel;
   private JPanel integrationPanel;
   private int lastCursor;

   public streamer_slider( ) 
   {
      setBorder( BorderFactory.createEmptyBorder(5,5,5,5) );
      setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
      setAlignmentX(Component.CENTER_ALIGNMENT);

      JLabel label = new JLabel("Streamline Settings");
      label.setAlignmentX(Component.CENTER_ALIGNMENT);
      label.setFont(new Font("SansSerif", Font.BOLD, 14));
      add(label);

      direction = new JRadioButton[3];

      direction[0] = new JRadioButton("X",true);
      direction[0].setActionCommand("X_");

      direction[1] = new JRadioButton("Y",false);
      direction[1].setActionCommand("Y_");

      direction[2] = new JRadioButton("Z",false);
      direction[2].setActionCommand("Z_");

      directionPanel = new JPanel();
      directionPanel.setBorder(BorderFactory.createCompoundBorder(
         BorderFactory.createTitledBorder("Direction"),
         BorderFactory.createEmptyBorder(-5,10,0,0))); //top,left,bottom,right
      directionPanel.setLayout(new GridLayout(0,1,0,-5)); //one column and as many rows as necessary.

      // add buttons to buttongroup and add buttons to panel..
      directionButtonGroup = new ButtonGroup();
      for( int i = 0; i < 3; i++)
      {
         direction[i].setEnabled(false);
         direction[i].setFont(new Font("SansSerif", Font.BOLD, 10));
         direction[i].addItemListener(this); 
         directionPanel.add(direction[i]);
         directionButtonGroup.add(direction[i]);
      }

      cursorType = new JRadioButton[4];

      cursorType[0] = new JRadioButton("none",true);
      cursorType[0].setActionCommand("NO_CURSOR");
      
      cursorType[1] = new JRadioButton("point",false);
      cursorType[1].setActionCommand("POINT_CURSOR");
      
      cursorType[2] = new JRadioButton("line",false);
      cursorType[2].setActionCommand("LINE_CURSOR");
      
      cursorType[3] = new JRadioButton("plane",false);
      cursorType[3].setActionCommand("PLANE_CURSOR");

      cursorSelectionPanel = new JPanel();
      cursorSelectionPanel.setBorder(BorderFactory.createCompoundBorder(
         BorderFactory.createTitledBorder("Cursor Selection"),
         BorderFactory.createEmptyBorder(-5,10,0,0))); //top,left,bottom,right
      // one column and as many rows as necessary...
      cursorSelectionPanel.setLayout(new GridLayout(0,1,0,-5));

      // add buttons to buttongroup and add buttons to panel..
      cursorSelectionButtonGroup= new ButtonGroup();
      for( int i = 0; i < 4; i++)
      {
         cursorType[i].setFont(new Font("SansSerif", Font.BOLD, 10));
         cursorType[i].addItemListener(this);
         cursorSelectionPanel.add(cursorType[i]);
         cursorSelectionButtonGroup.add(cursorType[i]);
      }

      integrationType= new JRadioButton[3];
      integrationType[0] = new JRadioButton("backward integration",false);
      integrationType[0].setActionCommand("BACKWARD_INTEGRATION");
      integrationType[1] = new JRadioButton("forward integration",false);
      integrationType[1].setActionCommand("FORWARD_INTEGRATION");
      integrationType[2] = new JRadioButton("both directions",true);
      integrationType[2].setActionCommand("TWO_DIRECTION_INTEGRATION");

      integrationPanel= new JPanel();
      integrationPanel.setBorder(BorderFactory.createCompoundBorder(
         BorderFactory.createTitledBorder("Integration Direction"),
         BorderFactory.createEmptyBorder(-5,10,0,0))); //top,left,bottom,right
    
      // one column and as many rows as necessary...
      integrationPanel.setLayout(new GridLayout(0,1,0,-5));

      // add buttons to buttongroup and add buttons to panel..
      integrationPanelButtonGroup = new ButtonGroup();
      for( int i = 0; i < 3; i++)
      {
         integrationType[i].setFont(new Font("SansSerif", Font.BOLD, 10));
         integrationType[i].addItemListener(this);
         integrationPanel.add(integrationType[i]);
         integrationPanelButtonGroup.add(integrationType[i]);
      }

      // initialize lastCursor (to none)...
      lastCursor = 0;

      Box cursorSelectionBox = Box.createVerticalBox();

      cursorSelectionBox.add(cursorSelectionPanel);
      cursorSelectionBox.add(directionPanel);
      cursorSelectionBox.add(integrationPanel);

      Box labelBox = Box.createHorizontalBox();
         label = new JLabel("numPts");
         label.setAlignmentX(Component.LEFT_ALIGNMENT);
         label.setFont(new Font("SansSerif", Font.BOLD, 10));
         labelBox.add(label);
      labelBox.add(Box.createHorizontalStrut(50));
         label = new JLabel("Size (%) ");
         label.setAlignmentX(Component.RIGHT_ALIGNMENT);
         label.setFont(new Font("SansSerif", Font.BOLD, 10));
      labelBox.add(label);

      streamer1 = new JSlider();
      streamer1.setPreferredSize(new Dimension(10, 1000));
      streamer1.setOrientation(JSlider.VERTICAL);
      streamer1.setMinimum(1);
      int maxNumStreamlineSeeds = 6; //TODO: should pass in from param file
      streamer1.setMaximum(maxNumStreamlineSeeds);
      int defaultNumPts = 2;
      streamer1.setValue(defaultNumPts);
      clt.obsref.min = defaultNumPts;
      streamer1.addChangeListener(this);
      streamer1.setMajorTickSpacing(1);
      streamer1.setMinorTickSpacing(1);
      streamer1.setPaintTicks(true);
      streamer1.setPaintLabels(true);
      streamer1.setSnapToTicks(true); // Snap to tick marks
      streamer1.setEnabled(false);

      streamer2 = new JSlider();
      streamer2.setPreferredSize(new Dimension(10, 1000));
      streamer2.setOrientation(JSlider.VERTICAL);
      streamer2.setMinimum(0);
      streamer2.setMaximum(100);
      int defaultSize = 20;
      streamer2.setValue(defaultSize);
      clt.obsref.max = defaultSize;
      streamer2.addChangeListener(this);
      streamer2.setMajorTickSpacing(10);
      streamer2.setMinorTickSpacing(1);
      streamer2.setPaintTicks(true);
      streamer2.setPaintLabels(true);
      streamer2.setEnabled(false);

      Box dualSliderBox = Box.createHorizontalBox();
      dualSliderBox.add(streamer1);
      dualSliderBox.add(Box.createHorizontalStrut(50));
      dualSliderBox.add(streamer2);

      Box streamerBox = Box.createVerticalBox();
      streamerBox.add(labelBox);
      streamerBox.add(Box.createVerticalStrut(5));
      streamerBox.add(dualSliderBox);

      Box centerBox = Box.createHorizontalBox();
      centerBox.add(streamerBox);
      centerBox.add(cursorSelectionBox);

      Single_slider propTimeSlider = new Single_slider( "Propag'n", 8,
                                  "CHANGE_PROPAGATION_TIME", 0, 100, 50, 0 );
      Single_slider intStepLenSlider = new Single_slider( "IntgStep", 8,
                                  "CHANGE_INT_STEP_LENGTH", 0, 100, 50, 0 );
      Single_slider stepLenSlider = new Single_slider( "Step", 8,
                                  "CHANGE_STEP_LENGTH", 0, 100, 50, 0 );
      centerBox.add(propTimeSlider);
      centerBox.add(intStepLenSlider);
      centerBox.add(stepLenSlider);

      add(centerBox);
      add(Box.createVerticalStrut(10));

      steamlineLoopButton = new JButton("Compute Streamlines");
      steamlineLoopButton.setActionCommand("STREAMLINES");
      steamlineLoopButton.addActionListener(this); 
      steamlineLoopButton.setSize(new Dimension(50,30));
      steamlineLoopButton.setEnabled(false);

      particleTrackButton = new JButton("Particle Tracking");
      particleTrackButton.setActionCommand("ANIMATED_STREAMLINES");
      particleTrackButton.addActionListener(this); 
      particleTrackButton.setSize(new Dimension(50,30));
      particleTrackButton.setEnabled(false);

      seedpointToggle = new JCheckBox("Use Last Seedpoints");
      seedpointToggle.setActionCommand("USE_LAST_STREAMLINE_SEEDPOINTS");
      seedpointToggle.addActionListener(this);
      seedpointToggle.setSelected(false);
      seedpointToggle.setEnabled(false);

      Box buttonBox = Box.createHorizontalBox();
      buttonBox.add(steamlineLoopButton);
      buttonBox.add(particleTrackButton);
      buttonBox.add(Box.createHorizontalStrut(10));
      buttonBox.add(seedpointToggle);

      add(buttonBox);
   }

   // Listen to the streamline sliders. 
   public void stateChanged(ChangeEvent e)
   {
      JSlider source = (JSlider)e.getSource();
      if (!source.getValueIsAdjusting()) 
      {
         int fps = (int)source.getValue();

         if (source == streamer1)
         {
            System.out.println("streamline seed number changed to "+fps);
         }
         else if (source == streamer2)
         {
            System.out.println("streamline size changed to "+fps);
         }
         else
         {
            System.out.println("ERROR: "+source);
         }

         //seedpointToggle.setSelected(false);
         update_cfdApp();
      }
   }

   private void update_cfdApp() 
   {
      // whenever streamline cursor changes, reset particleTrackButton to off
      particleTrackButton.setEnabled(false);

      clt.obsref.min = streamer1.getValue();
      clt.obsref.max = streamer2.getValue();

      String cmd = new String();
      if (cursorSelectionButtonGroup.getSelection().getActionCommand() == "NO_CURSOR" )
      {
         lastCursor = 0;
         cmd = cursorSelectionButtonGroup.getSelection().getActionCommand();
         steamlineLoopButton.setEnabled(false);
      }
      else if (cursorSelectionButtonGroup.getSelection().getActionCommand() == "POINT_CURSOR") 
      {
         lastCursor = 1;
         cmd = cursorSelectionButtonGroup.getSelection().getActionCommand();
         steamlineLoopButton.setEnabled(true);
      }
      else if (cursorSelectionButtonGroup.getSelection().getActionCommand() == "LINE_CURSOR")
      {
         lastCursor = 2;
         cmd = directionButtonGroup.getSelection().getActionCommand()
             + cursorSelectionButtonGroup.getSelection().getActionCommand();
         steamlineLoopButton.setEnabled(true);
      }
      else if (cursorSelectionButtonGroup.getSelection().getActionCommand() == "PLANE_CURSOR")
      {
         lastCursor = 3;
         cmd = directionButtonGroup.getSelection().getActionCommand()
             + cursorSelectionButtonGroup.getSelection().getActionCommand();
         steamlineLoopButton.setEnabled(true);
      }
      clt.obsref.id = my_mapper2.get_id("CHANGE_STREAMLINE_CURSOR");
      //System.out.println("CHANGE_STREAMLINE_CURSOR: "+cmd);
      clt.obsref.iso_value = my_mapper2.get_id(cmd);
      clt.obj_vis.send_id();
      //System.out.println("lastCursor = "+lastCursor);
   }

   public void actionPerformed(ActionEvent e) 
   {
      String request = new String();
      request = e.getActionCommand();
      System.out.println("actionPerformed request: "+request);

      if (e.getActionCommand().equals("USE_LAST_STREAMLINE_SEEDPOINTS"))
      {
         clt.obsref.id = my_mapper2.get_id(e.getActionCommand());
         if ( seedpointToggle.isSelected() == true )
            clt.obsref.iso_value = 1;
         else
            clt.obsref.iso_value = 0;
         System.out.println(e.getActionCommand()+": "+clt.obsref.iso_value);
      }
      else if (e.getActionCommand().equals("STREAMLINES"))
      {
         clt.obsref.id = my_mapper.get_id(e.getActionCommand());
         System.out.println(e.getActionCommand()+": "+clt.obsref.id);
         particleTrackButton.setEnabled(true);
         seedpointToggle.setEnabled(true);
      }
      else if (e.getActionCommand().equals("ANIMATED_STREAMLINES"))
      {
         clt.obsref.id = my_mapper.get_id(e.getActionCommand());
         System.out.println(e.getActionCommand()+": "+clt.obsref.id);
      }
      else
      {
         System.out.println("ERROR: "+e.getActionCommand());
      }

      clt.obj_vis.send_id();
   }

   public void itemStateChanged(ItemEvent e) 
   {  
      Object source = e.getItemSelectable();

      if (e.getStateChange() == ItemEvent.SELECTED)
      {
         for(int i = 0; i < 3; i++)
         {
            if (source == integrationType[i])
            {
               String request = new String();
               request = integrationPanelButtonGroup.getSelection()
                                                    .getActionCommand();
               System.out.println("request: "+request);
               clt.obsref.id = my_mapper2.get_id( request );
               clt.obj_vis.send_id();
               return;
            }
         }
      }

      // here whenever cursorDirection or cursorType radio buttons are clicked
      //System.out.println(cursorSelectionButtonGroup.getSelection().getActionCommand());

      if (cursorSelectionButtonGroup.getSelection().getActionCommand() == "NO_CURSOR" ||
          cursorSelectionButtonGroup.getSelection().getActionCommand() == "POINT_CURSOR")
      {
         // for no cursor or point cursor, turn off two sliders and direction
         streamer1.setEnabled(false);
         streamer2.setEnabled(false);
         for( int i = 0; i < 3; i++)
            direction[i].setEnabled(false);
      }
      else
      {
         // otherwise, turn on two sliders and the direction
         streamer1.setEnabled(true);
         streamer2.setEnabled(true);
         for( int i = 0; i < 3; i++)
            direction[i].setEnabled(true);
      }

      update_cfdApp();
   }
}

static class TeacherPanel extends JPanel
                          implements ActionListener
{
   private JPanel filePanel;
   private JScrollPane scrollPane;
   private JLabel label;
   private ButtonGroup fileGroup;
   private JRadioButton[] fileButton;
   private JButton recordButton;

   public TeacherPanel() 
   {
      javax.swing.border.TitledBorder titledBorder = 
                            BorderFactory.createTitledBorder("Stored Files");
      titledBorder.setTitleFont(new Font("SansSerif", Font.BOLD, 24));

      filePanel = new JPanel();
      filePanel.setBorder(BorderFactory.createCompoundBorder(
                  titledBorder, BorderFactory.createEmptyBorder(5,5,5,5)));
      filePanel.setLayout(new BoxLayout(filePanel, BoxLayout.Y_AXIS));

      scrollPane = new JScrollPane(filePanel);

      fileGroup = new ButtonGroup();
      fileButton = new JRadioButton[num_teacher+1];   // extra button for 'none'
      for( int i = 0; i < num_teacher; i++)
      {
         // set all buttons unselected at start...
         fileButton[ i ] = new JRadioButton(teacher_attrib[ i ],false);
         fileButton[ i ].setActionCommand("file"+i);
         fileButton[ i ].addActionListener(this);

         fileGroup.add( fileButton[ i ] );
         filePanel.add( fileButton[ i ] );
      }

      // define last button as "none"
      fileButton[ num_teacher ] = new JRadioButton("no file active",true);
      fileButton[ num_teacher ].setActionCommand("CLEAR_PFB_FILE");
      fileButton[ num_teacher ].addActionListener(this);

      fileGroup.add( fileButton[ num_teacher ] );
      filePanel.add( fileButton[ num_teacher ] );

      recordButton = new JButton("record current scene");
      recordButton.setActionCommand("RECORD_SCENE");
      recordButton.addActionListener(this); 
      recordButton.setAlignmentX(CENTER_ALIGNMENT);

      setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));

      add( scrollPane );
      add( recordButton );
   }

   public void actionPerformed(ActionEvent e) 
   {
      if (e.getActionCommand().equals("RECORD_SCENE"))
      {
         System.out.println(e.getActionCommand());
         clt.obsref.id = my_mapper2.get_id(e.getActionCommand());
      }
      else if ( e.getActionCommand().equals("CLEAR_PFB_FILE") )
      {
         System.out.println(e.getActionCommand());
         clt.obsref.id = my_mapper2.get_id(e.getActionCommand());
      }
      else
      {
         for( int i = 0; i < num_teacher; i++)
         {
            if ( e.getActionCommand().equals("file"+i) )
            {
               System.out.println("Selected stored binary file "+i);
               clt.obsref.id = my_mapper2.get_id("LOAD_PFB_FILE");
               // using zero-based file counting
               clt.obsref.teacher_state = (short)i;
            }
         }
      }

      clt.obj_vis.send_id();
   }
}

static class ParticlePanel extends JPanel
                           implements ActionListener
{
   private JPanel panel;
   private JLabel label;
   private JRadioButton[] bttn;
   private ButtonGroup bttnGroup;
   private JButton updateButton;
   private Double_slider scalarRangeSlider;
   private DatasetAndScalar dsPanel_1;
   private Single_slider scaleSlider;
   private Box box;

   public ParticlePanel( ) 
   {
      setLayout(new BoxLayout(this, BoxLayout.X_AXIS) );

      dsPanel_1 = new DatasetAndScalar( "vertex-based", 1 );

      box = Box.createVerticalBox();
      box.add( dsPanel_1 );

      panel = new JPanel();
      panel.setBorder( BorderFactory.createEmptyBorder(5,5,5,5) );
      panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));

      // Create a label for the group
      label = new JLabel("Particle Options");
      label.setFont(new Font("SansSerif", Font.BOLD, 14));
      panel.add(label);

      bttn = new JRadioButton[ 2 ];
      bttn[ 0 ] = new JRadioButton("view as a point cloud",true);
      bttn[ 1 ] = new JRadioButton("view as variably-sized spheres",false);

      bttnGroup = new ButtonGroup();
      for( int i = 0; i < 2; i++)
      {
         bttn[i].setFont(new Font("SansSerif", Font.BOLD, 10));
         bttn[i].setActionCommand("CHANGE_PARTICLE_VIEW_OPTION");
         bttn[i].addActionListener(this); 

         bttnGroup.add(bttn[i]);
         panel.add(bttn[i]);
      }

      updateButton = new JButton("Display Particles");
      updateButton.setActionCommand("Update");
      updateButton.addActionListener(this); 
      updateButton.setSize(new Dimension(50,30));
      panel.add(updateButton);

      box.add( panel );

      add( box );

      scalarRangeSlider = new Double_slider( "Scalar Range",
                                             "CHANGE_SCALAR_RANGE");
      add( scalarRangeSlider );

      scaleSlider = new Single_slider( "Sphere Size", 10,
                                  "CHANGE_SPHERE_SIZE", -100, 100, 0, 0 );
      disableContainer( scaleSlider );
      add(scaleSlider);
   }

   public void actionPerformed(ActionEvent e) 
   {
      clt.obsref.id = -1;

      if (e.getActionCommand().equals("Update"))
      {
         String cmd = new String();
         cmd = "PARTICLES";
         System.out.println(e.getActionCommand()+" "+cmd);
         clt.obsref.id = my_mapper.get_id( cmd );
      }

      if ( e.getActionCommand().equals("CHANGE_PARTICLE_VIEW_OPTION") )
      {
         clt.obsref.id = my_mapper2.get_id( e.getActionCommand() );
         for( int i = 0; i < 2; i++)
         {
            if ( bttn[ i ].isSelected() )
            {
               System.out.println( e.getActionCommand()+" "+i );
               clt.obsref.geo_state = i;
               break;
            }
         }

         if ( bttn[ 0 ].isSelected() )
         {  
            disableContainer( scaleSlider );
         }
         else if ( bttn[ 1 ].isSelected() )
         {  
            enableContainer( scaleSlider );
            clt.obsref.iso_value = scaleSlider.slide.getValue();
         }
      }

      if(clt.obsref.id != -1)
         clt.obj_vis.send_id();
   }
}

static class ViewLocationsPanel extends JPanel
                                implements ActionListener
{
   private JPanel panel;
   private JPanel panel_left;
   private JPanel panel_right;
   private int numButtons = 6;
   private JRadioButton[] bttn;
   private ButtonGroup bttnGroup;
   private JButton loadPointButton;
   private JButton writePointsButton;
   private JButton readPointsButton;
   private JButton moveButton;
   private JScrollPane scroller;
   //private Box box1;
   //private Box box2;

   public ViewLocationsPanel( ) 
   {
      setLayout(new BoxLayout(this, BoxLayout.X_AXIS) );

      panel = new JPanel();
      panel.setBorder( BorderFactory.createEmptyBorder(5,5,5,5) );
      panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));

      scroller = new JScrollPane(panel);
      scroller.setAlignmentX(CENTER_ALIGNMENT);
      scroller.setPreferredSize(new Dimension(400, 4000));

      bttn = new JRadioButton[ 6 ];
      bttnGroup = new ButtonGroup();
      for( int i = 0; i < numButtons; i++)
      {
         bttn[ i ] = new JRadioButton("Location_"+i,false);
         bttn[ i ].setFont(new Font("SansSerif", Font.BOLD, 12));
         bttn[ i ].setActionCommand("Location_"+i);
         //bttn[ i ].addActionListener(this); 

         bttnGroup.add(bttn[i]);
         panel.add(bttn[i]);
      }
      bttn[ 0 ].setSelected(true);

      moveButton = new JButton("Move to selected location");
      moveButton.setActionCommand("MOVE_TO_SELECTED_LOCATION");
      moveButton.addActionListener(this); 
      moveButton.setAlignmentX(Component.CENTER_ALIGNMENT);

      //box1 = Box.createVerticalBox();
      panel_left = new JPanel();
      panel_left.setBorder( BorderFactory.createEmptyBorder(5,5,5,5) );
      panel_left.setLayout(new BoxLayout(panel_left, BoxLayout.Y_AXIS));
      panel_left.add( scroller );
      panel_left.add( moveButton );

      //box2 = Box.createVerticalBox();
      panel_right = new JPanel();
      panel_right.setBorder( BorderFactory.createEmptyBorder(5,5,5,5) );
      panel_right.setLayout(new BoxLayout(panel_right, BoxLayout.Y_AXIS));

      loadPointButton = new JButton("Load point");
      loadPointButton.setActionCommand("LOAD_POINT");
      loadPointButton.addActionListener(this); 
      loadPointButton.setAlignmentX(Component.RIGHT_ALIGNMENT);
      panel_right.add( loadPointButton );
      panel_right.add(Box.createRigidArea(new Dimension(20,20)));

      writePointsButton = new JButton("Write points to file");
      writePointsButton.setActionCommand("WRITE_POINTS_TO_FILE");
      writePointsButton.addActionListener(this); 
      writePointsButton.setEnabled(false); 
      writePointsButton.setAlignmentX(Component.RIGHT_ALIGNMENT);
      panel_right.add( writePointsButton );
      panel_right.add(Box.createRigidArea(new Dimension(20,20)));

      readPointsButton = new JButton("Read points from file");
      readPointsButton.setActionCommand("READ_POINTS_FROM_FILE");
      readPointsButton.addActionListener(this); 
      readPointsButton.setAlignmentX(Component.RIGHT_ALIGNMENT);
      panel_right.add( readPointsButton );

      add( panel_left );
      add( panel_right );
   }

   public void actionPerformed(ActionEvent e) 
   {
      clt.obsref.id = -1;

      if (e.getActionCommand().equals("LOAD_POINT"))
      {
         clt.obsref.id = my_mapper2.get_id( e.getActionCommand() );
         System.out.println( e.getActionCommand() );
         writePointsButton.setEnabled(true); 
      }
      else if (e.getActionCommand().equals("WRITE_POINTS_TO_FILE"))
      {
         clt.obsref.id = my_mapper2.get_id( e.getActionCommand() );
         System.out.println( e.getActionCommand() );
      }
      else if (e.getActionCommand().equals("READ_POINTS_FROM_FILE"))
      {
         clt.obsref.id = my_mapper2.get_id( e.getActionCommand() );
         System.out.println( e.getActionCommand() );
      }
      else if ( e.getActionCommand().equals("MOVE_TO_SELECTED_LOCATION") )
      {
         clt.obsref.id = my_mapper2.get_id( e.getActionCommand() );
         for ( int i = 0; i < numButtons; i++)
         {
            if ( bttn[ i ].isSelected() )
            {
               System.out.println( e.getActionCommand()+" "+i );
               clt.obsref.iso_value = i;
               break;
            }
         }
      }

      if(clt.obsref.id != -1)
         clt.obj_vis.send_id();
   }
}

static class VectorPanel extends JPanel
                         implements ActionListener
{
   private JPanel VECTOR_PANEL;
   private JRadioButton[] vecButton;
   private JButton updateButton;
   private JScrollPane vectorScroller;
   private Box boxb;
   private Box boxc;
   private Double_slider vSlider;
   private Single_slider ratioSlider;
   private Single_slider scaleSlider;
   private JCheckBox scaleByVecMag;

   public VectorPanel( ) 
   {
      VECTOR_PANEL = new JPanel();
      VECTOR_PANEL.setBorder( BorderFactory.createEmptyBorder(5,5,5,5) );
      VECTOR_PANEL.setLayout(new BoxLayout(VECTOR_PANEL, BoxLayout.Y_AXIS));

      // the vector scroll panel will be populated with buttons by the datasets

      vectorScroller = new JScrollPane(VECTOR_PANEL);

      updateButton = new JButton("Update Visualization");
      updateButton.setActionCommand("Update");
      updateButton.addActionListener(listener); 
      updateButton.setSize(new Dimension(50,30));
      updateButton.setAlignmentX(Component.CENTER_ALIGNMENT);

      boxb = Box.createVerticalBox();
      boxb.add( vectorScroller );
      boxb.add( updateButton );

      vSlider = new Double_slider( "Vector Threshold",
                                   "CHANGE_VECTOR_THRESHOLD");
      // set the minimum at one percent to hide zero length vectors...
      vSlider.min_slide.setValue( 1 );
//System.out.println("vector slider min = "+vSlider.min_slide.getValue());

      ratioSlider = new Single_slider( "Vector Ratio", 10,
                                  "CHANGE_VECTOR_MASK_RATIO", 1, 15, 1 ,1 );

      scaleSlider = new Single_slider( "Vector Scale", 10,
                                  "CHANGE_VECTOR_SCALE", -100, 100, 0, 0 );

      scaleByVecMag = new JCheckBox("Scale by Vector Mag");
      scaleByVecMag.setFont(new Font("SansSerif", Font.BOLD, 10));
      scaleByVecMag.setActionCommand("SCALE_BY_VECTOR_MAGNITUDE");
      scaleByVecMag.addActionListener(this);
      scaleByVecMag.setAlignmentX(Component.CENTER_ALIGNMENT);
      scaleByVecMag.setSelected(false);

      boxc = Box.createVerticalBox();
      boxc.add(scaleSlider);
      boxc.add(scaleByVecMag);

      setLayout(new BoxLayout(this, BoxLayout.X_AXIS));
      add(boxb);
      add(Box.createRigidArea(new Dimension(5,0)));
      add(vSlider);
      add(ratioSlider);
      add(boxc);
   }

   public void actionPerformed(ActionEvent e) 
   {
      clt.obsref.id = -1;

      for( int i = 0; i < numVectorsInActiveDataset; i++)
      {
         if ( e.getActionCommand().equals("vector"+i) )
         {
            System.out.println("CHANGE_VECTOR "+i);
            clt.obsref.id = my_mapper2.get_id("CHANGE_VECTOR");
            clt.obsref.sc = i;         // using zero-based counting
            break;
         }
      }

      if (e.getActionCommand().equals("SCALE_BY_VECTOR_MAGNITUDE"))
      {
         System.out.println(e.getActionCommand());
         clt.obsref.id = my_mapper2.get_id(e.getActionCommand());
         JCheckBox source = (JCheckBox)e.getSource();
         clt.obsref.iso_value = 0;
         if ( source.isSelected() )
            clt.obsref.iso_value = 1;
         System.out.println("isSelected = "+clt.obsref.iso_value);
      }
 
      if(clt.obsref.id != -1)
         clt.obj_vis.send_id();
   }
}

static class ScalarPanel extends JPanel
                         implements ActionListener
{
   private JPanel SCALAR_PANEL;
   private JRadioButton[] r;
   private JButton updateButton;
   private JScrollPane scalarScroller;
   private Box box;
   private Double_slider scalarRangeSlider;

   public ScalarPanel()
   {
      SCALAR_PANEL = new JPanel();
      SCALAR_PANEL.setBorder( BorderFactory.createEmptyBorder(5,5,5,5) );
      SCALAR_PANEL.setLayout(new BoxLayout(SCALAR_PANEL, BoxLayout.Y_AXIS));

      // the scroll panel will be populated with buttons by the datasets

      scalarScroller = new JScrollPane( SCALAR_PANEL );

      JButton updateButton = new JButton("Update Visualization");
      updateButton.setActionCommand("Update");
      updateButton.addActionListener(listener); 
      updateButton.setSize(new Dimension(50,30));
      updateButton.setAlignmentX(Component.CENTER_ALIGNMENT);

      box = Box.createVerticalBox();
      box.add( scalarScroller );
      box.add( updateButton );

      scalarRangeSlider = new Double_slider( "Scalar Range",
                                             "CHANGE_SCALAR_RANGE");

      setLayout(new BoxLayout(this, BoxLayout.X_AXIS) );
      add( box );
      add( Box.createRigidArea(new Dimension(5,0)) );
      add( scalarRangeSlider );
   }

   public void actionPerformed(ActionEvent e) 
   {
      for( int i = 0; i < numScalarsInActiveDataset; i++)
      {
         if ( e.getActionCommand().equals("scalar"+i) )
         {
            System.out.println("Selected scalar "+i);
            scalarRangeSlider.min_slide.setValue(sc_min[ i ]);
            scalarRangeSlider.max_slide.setValue(sc_max[ i ]);
            clt.obsref.sc = i;         // using zero-based scalar counting
            clt.obsref.min = scalarRangeSlider.min_slide.getValue();
            clt.obsref.max = scalarRangeSlider.max_slide.getValue();
            //System.out.print("clt.obsref.sc = "+clt.obsref.sc+"\n");
            clt.obsref.id = my_mapper2.get_id("CHANGE_SCALAR");

            // make the datasets scalar scroll panel show the same thing...
            if      ( activeDataSetType == 0 )
            {
               datasetList.dsPanel_0.scalarButton[ i ].setSelected(true);
               datasetList.dsPanel_0.lastScalarIndex[datasetList.dsPanel_0.selectedDataSetOfGroup] = i;
            }
            else if ( activeDataSetType == 1 )
            {
               particlePanel.dsPanel_1.scalarButton[ i ].setSelected(true);
               particlePanel.dsPanel_1.lastScalarIndex[particlePanel.dsPanel_1.selectedDataSetOfGroup] = i;
            }
            else if ( activeDataSetType == 2 )
            {
               datasetList.dsPanel_2.scalarButton[ i ].setSelected(true);
               datasetList.dsPanel_2.lastScalarIndex[datasetList.dsPanel_2.selectedDataSetOfGroup] = i;
            }

            break;
         }
      }

      clt.obj_vis.send_id();
   }
}

static class GeometryPanel extends JPanel
                           implements ActionListener
{
   private JPanel panel;
   private JScrollPane scroller;
   private JCheckBox[] checkBox;
   private short num_geo;
   private String[] geo_attrib;
   private JButton updateGeometry;
   private JButton refreshButton;

   public GeometryPanel()
   {
      panel = new JPanel();
      panel.setBorder( BorderFactory.createEmptyBorder(5,5,5,5) );//top,left,bottom,right
      panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));

      scroller = new JScrollPane(panel);
      scroller.setAlignmentX(CENTER_ALIGNMENT);
      scroller.setPreferredSize(new Dimension(400, 4000));

      updateGeometry = new JButton("Update");
      updateGeometry.setActionCommand("UPDATE_GEOMETRY");
      updateGeometry.addActionListener(this);  
      updateGeometry.setAlignmentX(CENTER_ALIGNMENT);

      refreshButton = new JButton("refresh geometry list");
      refreshButton.setActionCommand("REFRESH_GEOMETRY_LIST");
      refreshButton.addActionListener(this); 
      refreshButton.setAlignmentX(CENTER_ALIGNMENT);

      setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
      add(scroller);
      add(Box.createVerticalStrut(10));

      Box buttonBox = Box.createHorizontalBox();
      buttonBox.setPreferredSize(new Dimension(400, 40));
      buttonBox.add( refreshButton );
      buttonBox.add( Box.createHorizontalStrut(50) );
      buttonBox.add( updateGeometry );
      add(buttonBox);
      add(Box.createVerticalStrut(5));

      RefreshGeometryList();
   }

   public void RefreshGeometryList()
   {
      panel.removeAll();

      num_geo = clt.obj_vis.testObs.get_geo_num();
      System.out.println("geo number: "+num_geo);
      if( num_geo > 0 )
      {
         geo_attrib = clt.obj_vis.testObs.get_geo_name();
         checkBox = new JCheckBox[ num_geo ];
      }

      for(int i = 0; i < num_geo; i++)
      {
         checkBox[ i ] = new JCheckBox(geo_attrib[ i ]);
         checkBox[ i ].setSelected(true);
         panel.add(checkBox[ i ]);
      }

      panel.invalidate();
      panel.validate();
      panel.repaint();

      updateGeometry.doClick();
   }

   public void actionPerformed(ActionEvent e) 
   {
      if (e.getActionCommand().equals("UPDATE_GEOMETRY"))
      {
         System.out.println(e.getActionCommand());
         clt.obsref.id = my_mapper2.get_id(e.getActionCommand());
         clt.obsref.geo_state = 0;
         for(int i = 0; i < num_geo; i++)
         {
            if ( checkBox[ i ].isSelected() )
               clt.obsref.geo_state += (int)my_math.pow( 2, i );
         }
         System.out.println("geo_state = "+clt.obsref.geo_state);
         clt.obj_vis.send_id();
      }
      else if (e.getActionCommand().equals("REFRESH_GEOMETRY_LIST"))
      {
         RefreshGeometryList();
      }
   }
}

static class TransientPanel extends JPanel
                            implements ActionListener
{
   private JPanel catPanel;
   private JRadioButton[] catBttn;
   private ButtonGroup catBttnGrp;
   private JPanel dirPanel;
   private JRadioButton[] dirBttn;
   private ButtonGroup dirBttnGrp;
   private int numTransientButtons = 4;
   private String cmd;

   public TransientPanel( )
   {
      catPanel = new JPanel();
      catPanel.setLayout(new GridLayout(2,3,5,-5));
      catPanel.setBorder(BorderFactory.createCompoundBorder(
                        BorderFactory.createTitledBorder("Category"),
                        BorderFactory.createEmptyBorder(-5,5,5,5)));

      catBttn = new JRadioButton[ numTransientButtons ];

      catBttn[ 0 ] = new JRadioButton("Contours on a plane",false);
      catBttn[ 0 ].setActionCommand("cat_TRANSIENT_CONTOUR");

      catBttn[ 1 ] = new JRadioButton("Vectors on a plane",false);
      catBttn[ 1 ].setActionCommand("cat_TRANSIENT_VECTOR");

      catBttn[ 2 ] = new JRadioButton("Contours & Vectors on a plane",false);
      catBttn[ 2 ].setActionCommand("cat_TRANSIENT_CONTOUR_AND_VECTOR");

      catBttn[ 3 ] = new JRadioButton("Droplets",false);
      catBttn[ 3 ].setActionCommand("cat_PARTICLE_TRANSIENT");

      // add buttons to buttongroup and add buttons to panel..
      catBttnGrp = new ButtonGroup();
      for( int i = 0; i < numTransientButtons; i++)
      {
         catBttn[ i ].setFont(new Font("SansSerif", Font.BOLD, 10));
         //catBttn[ i ].setBackground(new Color(1,1,1,0));
         catBttn[ i ].addActionListener(this);
         catBttnGrp.add( catBttn[ i ] );
         catPanel.add( catBttn[ i ] );
      }

      dirPanel = new JPanel();
      dirPanel.setBorder(BorderFactory.createCompoundBorder(
                        BorderFactory.createTitledBorder("Direction"),
                        BorderFactory.createEmptyBorder(-5,5,5,5)));
      dirPanel.setLayout(new GridLayout(0,1,0,-5)); //one column and as many rows as necessary.

      dirBttn = new JRadioButton[ 3 ];

      dirBttn[ 0 ] = new JRadioButton("X-plane",true);
      dirBttn[ 0 ].setActionCommand("dir_X_");

      dirBttn[ 1 ] = new JRadioButton("Y-plane",false);
      dirBttn[ 1 ].setActionCommand("dir_Y_");

      dirBttn[ 2 ] = new JRadioButton("Z-plane",false);
      dirBttn[ 2 ].setActionCommand("dir_Z_");

      // add buttons to buttongroup and add buttons to panel..
      dirBttnGrp = new ButtonGroup();
      for( int i = 0; i < 3; i++)
      {
         dirBttn[ i ].setFont(new Font("SansSerif", Font.BOLD, 10));
         dirBttn[ i ].addActionListener(this);
         dirBttnGrp.add( dirBttn[ i] );
         dirPanel.add( dirBttn[ i ] );
      }
      disableContainer( dirPanel );

      setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
      add( catPanel );
      add( dirPanel );

      add(Box.createVerticalStrut(5));

      System.out.println("Setting up progress bar with timesteps: "+timesteps);
      ProgressBar progressBar = new ProgressBar( timesteps - 1 );
      add( progressBar );
   }

   public void actionPerformed(ActionEvent e) 
   {
      // query the two button groups, after stripping off the first
      // four charcaters ("cat_" or "dir_")
      sg1 = transientPanel.dirBttnGrp.getSelection()
                          .getActionCommand().substring(4);
      sg2 = transientPanel.catBttnGrp.getSelection()
                          .getActionCommand().substring(4);

      cmd = new String();;
      if (sg2.equals("PARTICLE_TRANSIENT"))
         cmd = sg2;
      else if (sg2.startsWith("TRANSIENT"))
         cmd = sg1+sg2;
      System.out.println("cmd = "+cmd);

      clt.obsref.id = my_mapper2.get_id("SET_TRANSIENT_OPTIONS");
      clt.obsref.iso_value = my_mapper.get_id(cmd);
      clt.obj_vis.send_id();

      if ( cmd.endsWith("CONTOUR") || cmd.endsWith("VECTOR") )
      {
         enableContainer( dirPanel );

//System.out.println("SET_TRANSIENT_OPTIONS"+": "+e.getActionCommand().substring(4));

/*
//System.out.println("num dataset buttons = "+datasetList.dsPanel_2.numDatasetsOfType);
         // attempt to activate a dataset with poly data
         if ( datasetList.dsPanel_2.numDatasetsOfType > 0 )
            getSelection( datasetList.dsPanel_2.dsGroup ).doClick();
*/
      }
      else if (cmd.equals("PARTICLE_TRANSIENT"))
      {
       /*  clt.obsref.id = my_mapper2.get_id("SET_TRANSIENT_OPTIONS");
         clt.obsref.iso_value = 4;
         clt.obj_vis.send_id();*/
//System.out.println("num dataset buttons = "+particlePanel.dsPanel_1.numDatasetsOfType);
         // attempt to activate a dataset with particle data
         if ( particlePanel.dsPanel_1.numDatasetsOfType > 0 )
            getSelection( particlePanel.dsPanel_1.dsGroup ).doClick();

         disableContainer( dirPanel );
      }
   }
}

static class DatasetList extends JPanel
     
{
   //private int numDatasetCategories = 3;
   private DatasetAndScalar dsPanel_2;
   private DatasetAndScalar dsPanel_0;

   public DatasetList( )
   {
      // Create the panels for the different types of datasets.
      // Do the 3D mesh one last so that the 3D mesh set will be "active".
      dsPanel_2 = new DatasetAndScalar( "polydata", 2 );
      dsPanel_0 = new DatasetAndScalar( "3D mesh", 0 );

      setLayout(new BoxLayout(this, BoxLayout.X_AXIS));
      add( dsPanel_0 );
      add( dsPanel_2 );
      /*
      for(int i = 0; i < numDatasetCategories; i++) 
      {
         add( dsPanel[ i ] );
      }
      */
   }
}

static class ProgressBar extends JPanel 
{
   private JLabel label;
   private JProgressBar progressBar;
   private Timer timer;
   private JButton startButton;
   private JButton stopButton;
   private JButton resetButton;
   private JButton forwardButton;
   private JButton backwardButton;
   private short currentVal;
   private int my_pBarLength;

   public ProgressBar( int pBarLength ) 
   {
      my_pBarLength = pBarLength;
//System.out.println("3. pBarLength = "+pBarLength);
//System.out.println("4. current timesteps: "+clt.obj_vis.hello.get_timesteps());
      setBorder(BorderFactory.createCompoundBorder(
                      BorderFactory.createEtchedBorder(),
                      BorderFactory.createEmptyBorder(5,5,5,5)));
      setPreferredSize(new Dimension(400, 100));
      setMinimumSize(new Dimension(400, 100));
      setLayout(new BoxLayout(this,BoxLayout.Y_AXIS));

      label = new JLabel("Time Progress Bar");
      label.setFont(new Font("SansSerif", Font.BOLD, 10));
      label.setAlignmentX(Component.CENTER_ALIGNMENT);

      progressBar = new JProgressBar(0, pBarLength);
      progressBar.setValue(0);
      progressBar.setStringPainted(true);

      resetButton = new JButton("Reset");
      resetButton.setFont(new Font("SansSerif", Font.BOLD, 10));
      resetButton.setActionCommand("TRANSIENT_RESET");
      resetButton.setSize(new Dimension(50,30));
      resetButton.addActionListener(new ButtonListener()); 
      resetButton.setEnabled(false);

      backwardButton = new JButton("Backward");
      backwardButton.setFont(new Font("SansSerif", Font.BOLD, 10));
      backwardButton.setActionCommand("TRANSIENT_BACKWARD");
      backwardButton.setSize(new Dimension(50,30));
      backwardButton.addActionListener(new ButtonListener()); 
      backwardButton.setEnabled(false);

      startButton = new JButton("Start");
      startButton.setFont(new Font("SansSerif", Font.BOLD, 10));
      startButton.setActionCommand("TRANSIENT_START");
      startButton.addActionListener(new ButtonListener());
      startButton.setSize(new Dimension(50,30));
      startButton.setAlignmentX(Component.CENTER_ALIGNMENT);
      startButton.setEnabled(true);

      forwardButton = new JButton("Forward");
      forwardButton.setFont(new Font("SansSerif", Font.BOLD, 10));
      forwardButton.setActionCommand("TRANSIENT_FORWARD");
      forwardButton.setSize(new Dimension(50,30));
      forwardButton.addActionListener(new ButtonListener()); 
      forwardButton.setEnabled(false);

      stopButton = new JButton("Pause");
      stopButton.setFont(new Font("SansSerif", Font.BOLD, 10));
      stopButton.setActionCommand("TRANSIENT_STOP");
      stopButton.setSize(new Dimension(50,30));
      stopButton.addActionListener(new ButtonListener()); 
      stopButton.setEnabled(false);

      Box buttonBox = Box.createHorizontalBox();
      buttonBox.add(resetButton);
      buttonBox.add(Box.createHorizontalStrut(5));
      buttonBox.add(backwardButton);
      buttonBox.add(Box.createHorizontalStrut(5));
      buttonBox.add(startButton);
      buttonBox.add(Box.createHorizontalStrut(5));
      buttonBox.add(forwardButton);
      buttonBox.add(Box.createHorizontalStrut(5));
      buttonBox.add(stopButton);

      add(label);
      add(Box.createVerticalStrut(2));
      add(progressBar);
      add(buttonBox);

      //Create a timer.
      //first argument is time interval where ONE_SECOND = 1000;
      timer = new Timer(50, new ActionListener() 
      {
         int oldVal = -1;
         public void actionPerformed(ActionEvent e) 
         {
            currentVal = clt.obj_vis.testObs.getTimesteps();
            if ( (currentVal == 0) && (oldVal > 1) && (oldVal != my_pBarLength) )
            {
            // do nothing when currentVal makes dramatic jump back to zero
            }
            else if ( oldVal != currentVal )
            {
               System.out.println("updating progress bar with currentVal: "+currentVal);
               progressBar.setValue( currentVal );
               oldVal = currentVal;
            }
         }
      });

   }

   class ButtonListener implements ActionListener 
   {
      public void actionPerformed(ActionEvent e) 
      {
         System.out.println(e.getActionCommand());
         if (e.getActionCommand().equals("TRANSIENT_START"))
         {
            // make sure that the category is elected
            if ( getSelection( transientPanel.catBttnGrp ) == null )
               return;

            System.out.println("cmd = "+transientPanel.cmd);
            clt.obsref.id = my_mapper.get_id(transientPanel.cmd);
            clt.obj_vis.send_id();

            resetButton.setEnabled(true);
            startButton.setEnabled(false);
            stopButton.setEnabled(true);
            backwardButton.setEnabled(false);
            forwardButton.setEnabled(false);
            if ( ! timer.isRunning() ) timer.start();
         }
         else if (e.getActionCommand().equals("TRANSIENT_STOP"))
         {
            timer.stop();
            progressBar.setValue( currentVal );
            clt.obsref.id = my_mapper2.get_id(e.getActionCommand());
            System.out.println("clt.obsref.id = "+clt.obsref.id);
            clt.obj_vis.send_id();
            startButton.setEnabled(true);
            backwardButton.setEnabled(true);
            forwardButton.setEnabled(true);
            resetButton.setEnabled(true);
            stopButton.setEnabled(false);
         }
         else if (e.getActionCommand().equals("TRANSIENT_RESET"))
         {
            timer.stop();
            clt.obsref.id = my_mapper2.get_id(e.getActionCommand());
            System.out.println("clt.obsref.id = "+clt.obsref.id);
            clt.obj_vis.send_id();
            startButton.setEnabled(true);
            backwardButton.setEnabled(false);
            forwardButton.setEnabled(false);
            stopButton.setEnabled(false);
            resetButton.setEnabled(false);
            progressBar.setValue(progressBar.getMinimum());
         }
         else if (e.getActionCommand().equals("TRANSIENT_BACKWARD"))
         {
            System.out.println("currentVal = "+currentVal);
            clt.obsref.id = my_mapper2.get_id(e.getActionCommand());
            //System.out.println("clt.obsref.id = "+clt.obsref.id);
            clt.obj_vis.send_id();
            startButton.setEnabled(true);
            // start timer to let timer be responsible for progress bar position
            if ( ! timer.isRunning() ) timer.start();
         }
         else if (e.getActionCommand().equals("TRANSIENT_FORWARD"))
         {
            System.out.println("currentVal = "+currentVal);
            clt.obsref.id = my_mapper2.get_id(e.getActionCommand());
            //System.out.println("clt.obsref.id = "+clt.obsref.id);
            clt.obj_vis.send_id();
            startButton.setEnabled(true);
            // start timer to let timer be responsible for progress bar position
            if ( ! timer.isRunning() ) timer.start();
         }
      }
   }
}

/* 
The Java Developers Almanac 1.4
When you ask a button group for the currently selected radio button, it returns the selected radio button's model (rather than the selected radio button itself). Fortunately, the button group maintains the list of buttons and so you can iterate over this list looking for one with the same model. */

// This method returns the selected radio button in a button group
public static JRadioButton getSelection(ButtonGroup group)
{
   for (Enumeration e=group.getElements(); e.hasMoreElements(); )
   {
      JRadioButton b = (JRadioButton)e.nextElement();
      if (b.getModel() == group.getSelection())
      {
         return b;
      }
   }
   return null;
}

public static JRadioButton getNotSelection(ButtonGroup group)
{
   for (Enumeration e=group.getElements(); e.hasMoreElements(); )
   {
      JRadioButton b = (JRadioButton)e.nextElement();
      if (b.getModel() != group.getSelection())
      {
         return b;
      }
   }
   return null;
}


   static private void sleep(int t)
   {
      Thread thd = Thread.currentThread();

      try
      {
         thd.sleep(t);
      }
      catch(java.lang.InterruptedException ie)
      {
         System.out.println("InterruptedException:"+ie);
      }
   }

/*
public void itemStateChanged(ItemEvent e) 
{
   Object source = e.getItemSelectable();

   if (e.getStateChange() == ItemEvent.DESELECTED) 
   {
      for(int i = 0; i < num_geo; i++)
      {
         if (source == checkBox[i])
            clt.obsref.geo_state = clt.obsref.geo_state - (int)my_math.pow(2,i);
      }
   }
   else if (e.getStateChange() == ItemEvent.SELECTED)
   {
      for(int i = 0; i < num_geo; i++) 
      {
         if (source == checkBox[i])
            clt.obsref.geo_state = clt.obsref.geo_state + (int)my_math.pow(2,i);
      }
   }
}
*/

public void actionPerformed(ActionEvent e) 
{
   clt.obsref.id = -1;
   for(int i = 0; i < dest_num; i++)
      clt.recobj[i].id = -1;

   if (e.getActionCommand().equals("Update"))
   {
      // query the three button groups
      sg1 = vizPanel.direction_group.getSelection().getActionCommand().substring(4);
      sg2 = vizPanel.categoryButtonGroup.getSelection().getActionCommand().substring(4);
      sg3 = vizPanel.typeButtonGroup.getSelection().getActionCommand().substring(5);

      String cmd = new String();
      if (sg2.equals("ISO"))
         cmd = "ISOSURFACE";
      else if (sg2.equals("IMAGE_EX"))
         cmd = sg2;
      else if (sg2.equals("POLYDATA"))
         cmd = sg2;
      else 
         cmd = sg1+sg2+sg3;

      System.out.println(e.getActionCommand()+" "+cmd);
      clt.obsref.id = my_mapper.get_id(cmd);

      clt.obsref.pre_state = 0;
      if ( vizPanel.nearest.isSelected() )
         clt.obsref.pre_state = 1;

      clt.obsref.iso_value = isoPanel.iso_slide.getValue();
   }
   else if (e.getActionCommand().equals("UPDATE_SOUNDS"))
   {
      System.out.println(e.getActionCommand());
      clt.obsref.id = my_mapper2.get_id(e.getActionCommand());
      clt.obsref.iso_value = 0;
      for(int i = 0; i < numSounds; i++)
      {
         if ( sound[ i ].isSelected() )
            clt.obsref.iso_value += (int)my_math.pow( 2, i );
      }
      System.out.println("returning sound state = "+clt.obsref.iso_value);
   }
   else if (e.getActionCommand().equals("clear_draw"))
   {
      System.out.println(e.getActionCommand());
      drawing_panel.clear();
      clt.obsref.id =-1;
   }
   else if (e.getActionCommand().equals("done_draw"))
   {
      System.out.println(e.getActionCommand());
      drawing_panel.update_text();
      clt.obsref.id =-1;
   }
   else if (e.getActionCommand().equals("SEND_DRAW"))
   {
      System.out.println(e.getActionCommand());
      drawing_panel.convert_p();
      clt.recobj[dest_id].convert_p(drawing_panel.param);
      clt.recobj[dest_id].id = my_mapper2.get_id(e.getActionCommand());
   }
   else if (e.getActionCommand().equals("UPDATE_SEND_PARAM"))
   {
      System.out.println(e.getActionCommand());
      clt.recobj[dest_id].set_param(send_param_panel.get_param());
      clt.recobj[dest_id].id = my_mapper2.get_id(e.getActionCommand());
   }
   else if (e.getActionCommand().equals("get_perf"))
   {
      System.out.println(e.getActionCommand());
      String tmp = clt.obj_dst[dest_id].testObs.get_perf();
      ack_win awin = new ack_win(tmp,2,null);
      awin.start();
   } 
   else if (e.getActionCommand().equals("BLUE_MENU_TOGGLE"))
   {
      System.out.println(e.getActionCommand());
      clt.obsref.id = my_mapper2.get_id(e.getActionCommand());
   }
   else if (e.getActionCommand().equals("SCALAR_BAR_TOGGLE"))
   {
      System.out.println(e.getActionCommand());
      clt.obsref.id = my_mapper2.get_id(e.getActionCommand());
   }
   else if (e.getActionCommand().equals("REFRESH_DATASETS"))
   {
      System.out.println(e.getActionCommand());
      ReadDataSetInfo();
   }
   else if (e.getActionCommand().equals("CLEAR_ALL"))
   {
      System.out.println(e.getActionCommand());
      clt.obsref.id = my_mapper2.get_id(e.getActionCommand());
      streamerSlider.seedpointToggle.setSelected(false);
      streamerSlider.seedpointToggle.setEnabled(false);
      // on teacher tab, set button to "no files active"
      teacher.fileButton[num_teacher].setSelected(true);
   }
   else if (e.getActionCommand().equals("EXIT"))
   {
      System.out.println(e.getActionCommand());
      clt.obsref.id = my_mapper2.get_id(e.getActionCommand());
      clt.obj_vis.send_id(); 
      clt.disconnect();
      System.exit(1);
   }
   else
   {
      for( short i = 0; i < dest_num; i++)
      {
         if ( e.getActionCommand().equals("dest"+i) )
         {
            System.out.println("Select Destination"+i);
            dest_id = i;
         }
      }

      for( short i = 0; i < numSteadyStateDataSets; i++)
      {
         if ( e.getActionCommand().equals("steadyState"+i) )
         {
            System.out.println("CHANGE_STEADYSTATE_DATASET "+i);
            clt.obsref.id = my_mapper2.get_id("CHANGE_STEADYSTATE_DATASET");
            clt.obsref.iso_value = i;
            clt.obsref.min = scalarPanel.scalarRangeSlider.min_slide.getValue();
            clt.obsref.max = scalarPanel.scalarRangeSlider.max_slide.getValue();
         }
      }
   }

   if(clt.obsref.id != -1)
      clt.obj_vis.send_id();

   for(int i = 0; i < dest_num; i++)
   {
      if(clt.recobj[i].id != -1)
         clt.obj_dst[i].send_id();
   }

/*
// song
////////////////////////////////////////////////////////////
// stuff about update object list
////////////////////////////////////////////////////////////

  if (e.getActionCommand().equals("Update"))
   {
	while(!clt.obsref.obj_chg)
	{
	 System.out.println("Executing...");
	 sleep(500);
	}
	clt.obsref.obj_chg = false;

System.out.println("length"+clt.obsref.cur_obj.length);
cur_obj_name = new JLabel[clt.obsref.cur_obj.length];

	my_page.clear_list_page();

	for (int i = 0; i < clt.obsref.cur_obj.length; i++)
   	{
    	 cur_obj_name[i] = new JLabel("<html>\n"+"<font color=red size=-2>"+my_mapper.get_string(clt.obsref.cur_obj[i])+"</font>");
    	 my_page.add_scene_obj(cur_obj_name[i]);
   	}

	my_page.repaint();
	
   }
////////////////////////////////////////////////////////////
*/

}

static void read_dest() throws IOException
{
	String tmp_num,tmp_str;

	FileInputStream configFile = new FileInputStream("./config/dest.config");
	BufferedReader r = new BufferedReader(new InputStreamReader(configFile));
	
	tmp_num = r.readLine();
	if(tmp_num.startsWith("dest_num"))
	{
	 Integer tmp_int = new Integer(tmp_num.substring(9));
	 dest_num = tmp_int.shortValue();
	}

	dest = new String[dest_num];
	
	for ( int i = 0; i < dest_num; i++)
	{ 
	 tmp_str = r.readLine();
	 if(tmp_str.startsWith("dest_name"))
	  dest[i] = tmp_str.substring(10);
	}
	
	r.close();
}

public static void ReadDataSetInfo()
{
   // count the number of datasets, and for each get the list of scalars
   // and vectors...
   datasetNum = clt.obj_vis.testObs.get_sc_num();
   //datasetNum = clt.obj_vis.testObs.getNumDatasets();
   System.out.println("number of datasets: "+datasetNum);

   int numScalarsInFirstDataset = 0;
   int numVectorsInFirstDataset = 0;
   if( datasetNum > 0 )
   {
      scalarName = clt.obj_vis.testObs.update_scalar();  //all scalar names
//System.out.println("first scalar: "+scalarName[0]);

      vectorName = clt.obj_vis.testObs.update_vector();  //all vector names
//System.out.println("first vector: "+vectorName[0]);

      //allocate space for the arrays that store scalar min and max (0-100)
      int totalNumberOfScalars = clt.obj_vis.testObs.getTotalNumberOfScalars();
      sc_min = new short [ totalNumberOfScalars ];
      sc_max = new short [ totalNumberOfScalars ];

      // initialize to full scale...
      for( short i = 0; i < totalNumberOfScalars; i++)
      {
         sc_min[ i ] = 0;
         sc_max[ i ] = 100;
      }

      datasetNames = clt.obj_vis.testObs.get_dataset_names();
//System.out.println("first dataset: "+datasetNames[0]);

      datasetTypes = clt.obj_vis.testObs.get_dataset_types();
//System.out.println("first dataset type: "+datasetTypes[0]);

      numScalarsPerDataset = clt.obj_vis.testObs.get_num_scalars_per_dataset();
      numScalarsInFirstDataset = numScalarsPerDataset[ 0 ];
//System.out.println("numScalars in first dataset: "+numScalarsInFirstDataset);

      numVectorsPerDataset = clt.obj_vis.testObs.get_num_vectors_per_dataset();
      numVectorsInFirstDataset = numVectorsPerDataset[ 0 ];
//System.out.println("numVectors in first dataset: "+numVectorsInFirstDataset);

   }
   numScalarsInActiveDataset = numScalarsInFirstDataset;
   numVectorsInActiveDataset = numVectorsInFirstDataset;

   short postDataFlag = clt.obj_vis.testObs.getPostdataState();
   //System.out.println("postdata number: "+postDataFlag);

   hasXPostData = hasYPostData = hasZPostData = false;
   if ( postDataFlag == 1 || postDataFlag == 3 ||
        postDataFlag == 5 || postDataFlag == 7 )
      hasXPostData = true;
   if ( postDataFlag == 2 || postDataFlag == 3 ||
        postDataFlag == 6 || postDataFlag == 7 )
      hasYPostData = true;
   if ( postDataFlag == 4 || postDataFlag == 5 ||
        postDataFlag == 6 || postDataFlag == 7 )
      hasZPostData = true;

   // refresh the datasets ...
   datasetList.dsPanel_0.Refresh();
   
   particlePanel.dsPanel_1.Refresh();

   datasetList.dsPanel_2.Refresh();
}

public static void main(String args[])throws IOException 
{
   //System.out.println("args : " + args[ 0 ]);
   //System.out.println("args : " + args[ 1 ]);
   //System.out.println("args : " + args[ 2 ]);
   JFrame frame = new JFrame("JavaClient");
   clt.pass_main_frame(frame);

   read_dest();
   clt.set_vis();
   clt.set_dest(dest_num,dest);

   numSounds = clt.obj_vis.testObs.GetNumberOfSounds();
   System.out.println("number of sound files: "+numSounds);
   if( numSounds > 0 )
   {
      soundNameArray = clt.obj_vis.testObs.GetSoundNameArray();
      sound = new JCheckBox[ numSounds ];
   }

   num_teacher = clt.obj_vis.testObs.get_teacher_num();
   System.out.println("teacher number: "+num_teacher);
   if(num_teacher > 0)
   {
      teacher_attrib = clt.obj_vis.testObs.get_teacher_name();
   }
   clt.obsref.teacher_state = 0;   

   timesteps = clt.obj_vis.testObs.getTimesteps();
   System.out.println("1. timesteps: "+timesteps);

   my_page.init();

   ReadDataSetInfo();

   // click on category to force type button updates
   vizPanel.cat[0].doClick();

   frame.getContentPane().add( my_page, BorderLayout.CENTER);

   //frame.setSize(640, 480);
   frame.setSize(640, 540);   //stretched vertically to show buttons at bottom of visualization page
   frame.setVisible(true);

   frame.addWindowListener(new WindowAdapter() {
      public void windowClosing(WindowEvent e) {
      clt.disconnect();
      System.exit(0);}
      });
}

static void disableContainer(Container c)
{
   //System.out.println("disableContainer");
   c.setEnabled(false);
   Component[] components = c.getComponents();
   for(int i=0; i<components.length; i++)
   {
      //System.out.println("disableContainer: "+i);
      components[i].setEnabled(false);
      if(components[i] instanceof Container)
         disableContainer((Container)components[i]);
   }
}

static void enableContainer(Container c)
{
   //System.out.println("enableContainer");
   c.setEnabled(true);
   Component[] components = c.getComponents();
   for(int i=0; i<components.length; i++)
   {
      //System.out.println("enableContainer: "+i);
      components[i].setEnabled(true);
      if(components[i] instanceof Container)
         enableContainer((Container)components[i]);
   }
}

}
