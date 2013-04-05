package de.fhg.iese.hats.configurator;

import de.ovgu.featureide.fm.ui.editors.configuration.ConfigurationEditorPage;
import de.ovgu.featureide.fm.ui.editors.configuration.IConfigurationEditorPage;


import de.ovgu.featureide.fm.ui.editors.configuration.*;



import java.beans.PropertyChangeEvent;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;

import javax.swing.JLabel;
import javax.swing.JPanel;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.eclipse.swt.widgets.TaskItem;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.TreeItem;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.progress.UIJob;

import de.ovgu.featureide.fm.core.Feature;
import de.ovgu.featureide.fm.core.configuration.Configuration;
import de.ovgu.featureide.fm.core.configuration.SelectableFeature;
import de.ovgu.featureide.fm.core.configuration.Selection;
import de.ovgu.featureide.fm.core.configuration.TreeElement;
import de.ovgu.featureide.fm.ui.FMUIPlugin;

import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import abs.frontend.parser.*;

import java.io.*;

/**
* Displays the tree for common configuration selection at the configuration editor
* 
* @author Jens Meinicke
* @author Hannes Smurawsky
*/
public class ConfigurationInput extends ConfigurationEditorPage implements
IConfigurationEditorPage {

	private static final String PAGE_TEXT = "HATS Configuration";
	private static final String ID = FMUIPlugin.PLUGIN_ID + "ConfigurationPage";
	public static ArrayList<ArrayList<String>> arlSolutions = new ArrayList<ArrayList<String>>(); 
	private Tree tree;
	
	private Color gray = new Color(null,140,140,140);
	private Color green = new Color(null,0,140,0);
	private Color blue = new Color(null,0,0,200);
	
	private Font treeItemStandardFont = new Font(null, "Arial", 8, SWT.NORMAL);
	private Font treeItemSpecialFont = new Font(null,"Arial", 8, SWT.BOLD);

	/**
	 * Contains the TreeItems for coloring.
	 */
	private HashMap<SelectableFeature, TreeItem> items = new HashMap<SelectableFeature, TreeItem>();	
	/**
	 * Contains the features to be checked at coloring thread.
	 */
	private LinkedList<SelectableFeature> features = new LinkedList<SelectableFeature>();
	/**
	 * Stops the coloring thread if true.
	 */
	private boolean returnFormThread = false;
	private Job job_color;
	public void cancelColorJob() {
		returnFormThread = true;
	}
	
	private boolean selectionChanged = true;
	
	private boolean initialized = false;
	
	private boolean IsMinSelected = false;
	private boolean IsMaxSelected = false;
	private boolean IsMinDistantSelected = false;
	private boolean IsLeastCostSelected = false;
	private boolean IsMaxCostSelected = false;
	private boolean IsMaxCostConstraintSelected = false;
	private boolean IsMaxPerformantSelected = false;
	private boolean IsMostSecureSelected = false;
	private boolean IsQualitySelected = false;
	
	public String mtvlPath = "C:\\Users\\arif\\Desktop\\replicationsystem";
	private ArrayList<String> arlSelectedFeatures = new ArrayList<String>();
	
	private LinkedList<String> hiddenFeatures;
	
	public void updateTree(){
		if (errorMessage())
			refreshTree();
	}

	@Override
	public void createPartControl(Composite parent) {	
		
		Composite composite = new Composite(parent, SWT.BORDER);
		
		GridLayout gridLayout = new GridLayout();
	    gridLayout.numColumns = 2;
	    gridLayout.makeColumnsEqualWidth = true;
		composite.setLayout(gridLayout);
		
		
		Label labelBasic = new Label(composite, SWT.HORIZONTAL);
		labelBasic.setText("Basic Configuration");
		labelBasic.setFont( new Font(parent.getDisplay(),"Calibri", 16, SWT.NORMAL));
		
		
		GridData gridData = new GridData();
		gridData.horizontalSpan = 2;
	    labelBasic.setLayoutData(gridData);
	    

	    SelectionListener listenerRadioButtons = new SelectionListener () {
		    //public void handleEvent (Event event) {
			public void widgetSelected(SelectionEvent event){
		      Button button = (Button) event.widget;
		      	System.out.println("inside listener");
			  	IsMinSelected = false;
			  	IsMaxSelected = false;
			  	IsMinDistantSelected = false;
			  	IsLeastCostSelected = false;
			  	IsMaxCostSelected = false;
			  	IsMaxCostConstraintSelected = false;
			  	IsMaxPerformantSelected = false;
			  	IsMostSecureSelected = false;
			  	IsQualitySelected = false;
			  	
			  	if(button.getSelection() == false){
			  		return;
			  	}
	            if ((button.getStyle () & SWT.RADIO) != 0) button.setSelection (false);
	          
	            button.setSelection (true);
	            if(button.getText().trim() == "Minimum Configuration")
	            {
	            	IsMinSelected = true;	
	            	
	            }
	            else if(button.getText().trim() == "Maximum Configuration")
	            {
	            	IsMaxSelected = true;	
	            }
	            else if(button.getText().trim() == "Minimum Distant Valid Configuration")
	            {
	            	IsMinDistantSelected = true;	
	            }
	            else if(button.getText().trim() == "Least Cost Configuration")
	            {
	            	IsLeastCostSelected = true;	
	            }
	            else if(button.getText().trim() == "Maximum Cost Configuration")
	            {
	            	IsMaxCostSelected = true;	
	            }
	            else if(button.getText().trim() == "Maximal Configuration within Cost Constraint")
	            {
	            	IsMaxCostConstraintSelected = true;	
	            }
	            else if(button.getText().trim() == "Least Response Time Configuration")
	            {
	            	IsMaxPerformantSelected = true;	
	            }
	            else if(button.getText().trim() == "Minimum Memory Consumption Configuration")
	            {
	            	IsMostSecureSelected = true;	
	            }
	            else if(button.getText().trim() == "Customized")
	            {
	            	IsQualitySelected = true;	
	            }
	            System.out.print("\nHello from Listener : " + button.getText());
	            
		    }

			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
				// TODO Auto-generated method stub
				
			}
		  };
		  
		  
		Button btnMinConfig = new Button(composite, SWT.RADIO);
		btnMinConfig.setText("Minimum Configuration");		
		//btnMinConfig.addListener(SWT.SELECTED, listenerRadioButtons);
		btnMinConfig.addSelectionListener(listenerRadioButtons);
		btnMinConfig.setLayoutData(gridData);
//		btnMinConfig.setSelection(true);
//		btnMinConfig.addSelectionListener(new SelectionListener(){
//			public void widgetSelected(SelectionEvent e){
//				System.out.println("Minimum Configuration Selected");
//				
//				IsMinSelected = true;
//				
//			}
//
//			@Override
//			public void widgetDefaultSelected(SelectionEvent e) {
//				// TODO Auto-generated method stub
//				
//			}
//		});

		
		Button btMaxConfig = new Button(composite, SWT.RADIO);
		btMaxConfig.setText("Maximum Configuration");		
		//btMaxConfig.addSelectionListener(SWT.SELECTED, listenerRadioButtons);
		btMaxConfig.addSelectionListener(listenerRadioButtons);

		Label emptybtMaxConfig = new Label(composite, SWT.HORIZONTAL);
		emptybtMaxConfig.setText("");
		
		Button btnMinDistant = new Button(composite, SWT.RADIO);
		btnMinDistant.setText("Minimum Distant Valid Configuration");		
		//btnMinDistant.addListener(SWT.SELECTED, listenerRadioButtons);		
		btnMinDistant.addSelectionListener(listenerRadioButtons);
		
		
		Label emptybtnMinDistant = new Label(composite, SWT.HORIZONTAL);
		emptybtnMinDistant.setText("");
		
		
		Label labelCostaware = new Label(composite, SWT.HORIZONTAL);
		labelCostaware.setText("Cost-aware Configuration");
		labelCostaware.setFont( new Font(parent.getDisplay(),"Calibri", 16, SWT.NORMAL));
		
		Label emptyCostaware = new Label(composite, SWT.HORIZONTAL);
		emptyCostaware.setText("");
		
		Button btnLeastCost = new Button(composite, SWT.RADIO);
		btnLeastCost.setText("Least Cost Configuration");		
		//btnLeastCost.addListener(SWT.SELECTED, listenerRadioButtons);
		btnLeastCost.addSelectionListener(listenerRadioButtons);
		Label emptybtnLeastCost = new Label(composite, SWT.HORIZONTAL);
		emptybtnLeastCost.setText("");
		
		Button btnMaxCost= new Button(composite, SWT.RADIO);		
		btnMaxCost.setText("Maximum Cost Configuration");		
		//btnMaxCost.addListener(SWT.SELECTED, listenerRadioButtons);
		btnMaxCost.addSelectionListener(listenerRadioButtons);
		Label emptybtnMaxCost = new Label(composite, SWT.HORIZONTAL);
		emptybtnMaxCost.setText("");
		
		
		
		Button btnMaximalCostConstraint= new Button(composite, SWT.RADIO);		
		btnMaximalCostConstraint.setText("Maximal Configuration within Cost Constraint");		
		//btnMaximalCostConstraint.addListener(SWT.SELECTED, listenerRadioButtons);
		btnMaximalCostConstraint.addSelectionListener(listenerRadioButtons);
		
//		Label labelCostConstraint = new Label(composite, SWT.NULL);
//		labelCostConstraint.setText("Cost");				
		final Text txtCostConstraint = new Text(composite, SWT.SINGLE | SWT.BORDER );
		//txtCostConstraint.setLayoutData(new GridData(GridData.GRAB_HORIZONTAL));
		
		Label labelQualityaware = new Label(composite, SWT.HORIZONTAL);
		labelQualityaware.setText("Quality-aware Configuration");
		labelQualityaware.setFont( new Font(parent.getDisplay(),"Calibri", 16, SWT.NORMAL));
		Label emptyQualityaware = new Label(composite, SWT.HORIZONTAL);
		emptyQualityaware.setText("");
		
		
		Button btnMaximumPerformant = new Button(composite, SWT.RADIO);
		btnMaximumPerformant.setText("Least Response Time Configuration");		
		//btnMaximumPerformant.addListener(SWT.SELECTED, listenerRadioButtons);
		btnMaximumPerformant.addSelectionListener(listenerRadioButtons);
		Label emptyMaximumPerformant = new Label(composite, SWT.HORIZONTAL);
		emptyMaximumPerformant.setText("");
		
		Button btnSecure = new Button(composite, SWT.RADIO);		
		btnSecure.setText("Minimum Memory Consumption Configuration");		
		//btnSecure.addListener(SWT.SELECTED, listenerRadioButtons);
		btnSecure.addSelectionListener(listenerRadioButtons);
		Label emptybtnSecure = new Label(composite, SWT.HORIZONTAL);
		emptybtnSecure.setText("");
		
		Button btnQualityPreference = new Button(composite, SWT.RADIO);		
		btnQualityPreference.setText("Customized");		
		//btnQualityPreference.addListener(SWT.SELECTED, listenerRadioButtons);
		btnQualityPreference.addSelectionListener(listenerRadioButtons);
		btnQualityPreference.setLayoutData(gridData);
		// For Label - Textbox to appear...
		
		Label labelCost = new Label(composite, SWT.NULL);
		labelCost.setText("Cost");		
		
		final Text txtCost = new Text(composite, SWT.SINGLE | SWT.BORDER);
		//GridData expGridData = new GridData();
		
        //expGridData.horizontalAlignment = GridData.FILL;
		//expGridData.grabExcessHorizontalSpace = true;
        //expGridData.grabExcessVerticalSpace = true;
		//txtCost.setLayoutData(expGridData); 
	    //txtCost.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_END));	    
	    
	    Label labelQuality = new Label(composite, SWT.NULL);
	    labelQuality.setText("Quality");
	    //labelQuality.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_BEGINNING));
	    
	    Label labelImportance = new Label(composite, SWT.NULL);
	    labelImportance.setText("% of Importance");
	    //labelImportance.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_END));
	    
	    Label labelPerformance = new Label(composite, SWT.NULL);
	    labelPerformance.setText("Performance");
	    //labelPerformance.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_BEGINNING));
	    
	    final Text txtPerformance = new Text(composite, SWT.SINGLE | SWT.BORDER);
	    //txtPerformance.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_END));	    
	    
	    Label labelMemoryConsumption = new Label(composite, SWT.NULL);
	    labelMemoryConsumption.setText("    Memory Consumption");
	    //labelMemoryConsumption.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_BEGINNING));
	    
	    final Text txtMemoryConsumption = new Text(composite, SWT.SINGLE | SWT.BORDER);
	    //txtMemoryConsumption.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_END));	    
	    
	    Label labelResponseTime = new Label(composite, SWT.NULL);
	    labelResponseTime.setText("    Response Time");
	    //labelResponseTime.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_BEGINNING));
	    
	    final Text txtResponseTime = new Text(composite, SWT.SINGLE | SWT.BORDER);
	    //txtResponseTime.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_END));	    
	    
	    Label labelSecurity = new Label(composite, SWT.NULL);
	    labelSecurity.setText("Security");
	    //labelSecurity.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_BEGINNING));
	    
	    final Text txtSecurity = new Text(composite, SWT.SINGLE | SWT.BORDER);
	    //txtSecurity.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_END));	    
	    
	    Label labelConfidentiality = new Label(composite, SWT.NULL);
	    labelConfidentiality.setText("    Confidentiality");
	    //labelMemoryConsumption.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_BEGINNING));
	    
	    final Text txtConfidentiality = new Text(composite, SWT.SINGLE | SWT.BORDER);

	    Label labelIntegrity = new Label(composite, SWT.NULL);
	    labelIntegrity.setText("    Integrity");
	    //labelMemoryConsumption.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_BEGINNING));
	    
	    final Text txtIntegrity = new Text(composite, SWT.SINGLE | SWT.BORDER);

	    Label labelNonRepudiation = new Label(composite, SWT.NULL);
	    labelNonRepudiation.setText("    Non-repudiation");
	    //labelMemoryConsumption.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_BEGINNING));
	    
	    final Text txtNonRepudiation = new Text(composite, SWT.SINGLE | SWT.BORDER);

	    Label labelAccountability = new Label(composite, SWT.NULL);
	    labelAccountability.setText("    Accountability");
	    //labelMemoryConsumption.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_BEGINNING));
	    
	    final Text txtAccountability = new Text(composite, SWT.SINGLE | SWT.BORDER);

	    Label labelAuthentication = new Label(composite, SWT.NULL);
	    labelAuthentication.setText("    Authenticity");
	    //labelMemoryConsumption.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_BEGINNING));
	    
	    final Text txtAuthentication = new Text(composite, SWT.SINGLE | SWT.BORDER);


	    
	    
	    
	    
	    
	    Button btnConfigure = new Button(composite, SWT.PUSH);
	    btnConfigure.setText("Configure");
	    btnConfigure.setLayoutData(new GridData(SWT.CENTER, SWT.CENTER, false, false));;
		
		final ConfigurationResult resultPage = new ConfigurationResult();
		
		btnConfigure.addSelectionListener(new SelectionListener(){
			public void widgetSelected(SelectionEvent e){
				
				if(IsMinSelected)
				{
					Main oMain = new Main();
					oMain.mainMethod("-solve",mtvlPath);
					
					arlSolutions = oMain.GetAllSolutions();
					System.out.print("\nNumber of Solutions: " + arlSolutions.size());
									 
					for (IConfigurationEditorPage page : configurationEditor.getExtensionPages()) {
						System.out.println("\nPage Text: " + page.getPageText());
						if(page.getPageText().equals("Configuration Result")){
							ConfigurationResult cr = (ConfigurationResult)page;
							String[][] str = {{"Site", "ClientNR"}, {"Seq", "Concur", "File"}};							
							cr.setResult(str);																			 
							cr.showResult();						
							setInput(configurationEditor.configuration);
						}
					}
				}
				else if (IsMaxSelected)
				{
					System.out.println("\nMaximum Configuration is Selected");
					
					Main oMain = new Main();
					oMain.mainMethod("-maxProduct",mtvlPath);
					
					arlSolutions = oMain.GetAllSolutions();
					System.out.print("\nNumber of Solutions: " + arlSolutions.size());
									 
					for (IConfigurationEditorPage page : configurationEditor.getExtensionPages()) {
						System.out.println("\nPage Text: " + page.getPageText());
						if(page.getPageText().equals("Configuration Result")){
							ConfigurationResult cr = (ConfigurationResult)page;
							String[][] str = {{"Site", "ClientNR"}, {"Seq", "Concur", "File"}};							
							cr.setResult(str);	 
							cr.showResult();						
							setInput(configurationEditor.configuration);
						}
					}		
				}
				else if (IsMinDistantSelected)
				{
					try{						  
						  // Create file	
						  FileWriter fstream = new FileWriter(mtvlPath + "\\out.abs");
						  BufferedWriter out = new BufferedWriter(fstream);
						  out.write("product HATSPID(");
					
						  System.out.println("\nSelected Features: " + arlSelectedFeatures.size());
						  
						  for(int i=0;i<arlSelectedFeatures.size();i++)
						  {
							  out.write(arlSelectedFeatures.get(i).trim());
							  
							  if(i != (arlSelectedFeatures.size() - 1))
							  out.write(",");
						  }
						  
						  out.write(");");
						  out.close();  
					}
					catch (Exception e1)
						{//Catch exception if any
						  System.err.println("Error: " + e1.getMessage());
						}
									
					System.out.println("\nMinimum Distant Valid Configuration is Selected");
					Main oMain = new Main();
					oMain.mainMethod("-minWith=HATSPID",mtvlPath);
					
					arlSolutions = oMain.GetAllSolutions();
					System.out.print("\nNumber of Solutions: " + arlSolutions.size());
									 
					for (IConfigurationEditorPage page : configurationEditor.getExtensionPages()) {
						System.out.println("\nPage Text: " + page.getPageText());
						if(page.getPageText().equals("Configuration Result")){
							ConfigurationResult cr = (ConfigurationResult)page;
							String[][] str = {{"Site", "ClientNR"}, {"Seq", "Concur", "File"}};							
							cr.setResult(str);	 
							cr.showResult();						
							setInput(configurationEditor.configuration);
						}
					}		
					
				}
				else if(IsLeastCostSelected)
				{
					System.out.println("\nSolveAll option is Selected");
					Main oMain = new Main();
					oMain.mainMethod("-min=ReplicationSystem.cost",mtvlPath);
					
					arlSolutions = oMain.GetAllSolutions();
					System.out.print("\nNumber of Solutions: " + arlSolutions.size());
									 
					for (IConfigurationEditorPage page : configurationEditor.getExtensionPages()) {
						System.out.println("\nPage Text: " + page.getPageText());
						if(page.getPageText().equals("Configuration Result")){
							ConfigurationResult cr = (ConfigurationResult)page;
							String[][] str = {{"Site", "ClientNR"}, {"Seq", "Concur", "File"}};
							//String[] s =  {"French", "Dutch"};
							cr.setResult(str);						
							//cr.selectFeature(tree.getTopItem(),s);						 
							cr.showResult();						
							setInput(configurationEditor.configuration);
						}
					}					
				}
				else if(IsMaxCostSelected)
				{
					System.out.println("\nSolveAll option is Selected");
					Main oMain = new Main();
					oMain.mainMethod("-max=ReplicationSystem.cost",mtvlPath);
					
					arlSolutions = oMain.GetAllSolutions();
					System.out.print("\nNumber of Solutions: " + arlSolutions.size());
									 
					for (IConfigurationEditorPage page : configurationEditor.getExtensionPages()) {
						System.out.println("\nPage Text: " + page.getPageText());
						if(page.getPageText().equals("Configuration Result")){
							ConfigurationResult cr = (ConfigurationResult)page;
							String[][] str = {{"Site", "ClientNR"}, {"Seq", "Concur", "File"}};
							//String[] s =  {"French", "Dutch"};
							cr.setResult(str);						
							//cr.selectFeature(tree.getTopItem(),s);						 
							cr.showResult();						
							setInput(configurationEditor.configuration);
						}
					}					
				}
				else if(IsMaxCostConstraintSelected)
				{
					System.out.println("\nMaximal Configuration within Cost Constraint option is Selected");
					Main oMain = new Main();					
					int iCostConstraint = Integer.parseInt(txtCostConstraint.getText().trim());
					oMain.maximumCostConstraint(iCostConstraint);
					
					
				}
				else if(IsMaxPerformantSelected){
					System.out.println("\nMaximum Performant option is Selected");
					Main oMain = new Main();
					oMain.mainMethod("-min=ReplicationSystem.im_responseTime",mtvlPath);
					
					arlSolutions = oMain.GetAllSolutions();
					System.out.print("\nNumber of Solutions: " + arlSolutions.size());
									 
					for (IConfigurationEditorPage page : configurationEditor.getExtensionPages()) {
						System.out.println("\nPage Text: " + page.getPageText());
						if(page.getPageText().equals("Configuration Result")){
							ConfigurationResult cr = (ConfigurationResult)page;
							String[][] str = {{"Site", "ClientNR"}, {"Seq", "Concur", "File"}};
							//String[] s =  {"French", "Dutch"};
							cr.setResult(str);						
							//cr.selectFeature(tree.getTopItem(),s);						 
							cr.showResult();						
							setInput(configurationEditor.configuration);
						}
					}					
				}
				else if(IsMostSecureSelected){
					System.out.println("\nMaximum Performant option is Selected");
					Main oMain = new Main();
					oMain.mainMethod("-min=ReplicationSystem.im_memoryConsumption",mtvlPath);
					
					arlSolutions = oMain.GetAllSolutions();
					System.out.print("\nNumber of Solutions: " + arlSolutions.size());
									 
					for (IConfigurationEditorPage page : configurationEditor.getExtensionPages()) {
						System.out.println("\nPage Text: " + page.getPageText());
						if(page.getPageText().equals("Configuration Result")){
							ConfigurationResult cr = (ConfigurationResult)page;
							String[][] str = {{"Site", "ClientNR"}, {"Seq", "Concur", "File"}};
							//String[] s =  {"French", "Dutch"};
							cr.setResult(str);						
							//cr.selectFeature(tree.getTopItem(),s);						 
							cr.showResult();						
							setInput(configurationEditor.configuration);
						}
					}					
				}
				else if(IsQualitySelected)
				{
					System.out.println("\nBased on Quality Preference option is selected");
					
					HashMap<String, Object> hmPrefernces = new HashMap<String, Object>();
					
					if(txtCost.getText().trim().length() != 0)
						hmPrefernces.put("Cost", Integer.parseInt(txtCost.getText()));
					else
						hmPrefernces.put("Cost", Integer.parseInt("0"));
					
					if(txtPerformance.getText().trim().length() != 0)
						hmPrefernces.put("Performance", Integer.parseInt(txtPerformance.getText()));
					else
						hmPrefernces.put("Performance", Integer.parseInt("0"));
					
					if(txtMemoryConsumption.getText().trim().length() != 0)
						hmPrefernces.put("Memory_Consumption", Integer.parseInt(txtMemoryConsumption.getText()));
					else
						hmPrefernces.put("Memory_Consumption", Integer.parseInt("0"));
					
					if(txtResponseTime.getText().trim().length() != 0)
						hmPrefernces.put("Response_Time", Integer.parseInt(txtResponseTime.getText()));
					else
						hmPrefernces.put("Response_Time", Integer.parseInt("0"));
					
					if(txtSecurity.getText().trim().length() != 0)
						hmPrefernces.put("Security", Integer.parseInt(txtSecurity.getText()));
					else
						hmPrefernces.put("Security", Integer.parseInt("0"));
					
					Main oMain = new Main();
					oMain.QualityPreferences(hmPrefernces);	
					
					oMain.mainMethod("-max=ReplicationSystem.im_responseTime",mtvlPath);
					
					arlSolutions = oMain.GetAllSolutions();
					System.out.print("\nNumber of Solutions: " + arlSolutions.size());
									 
					for (IConfigurationEditorPage page : configurationEditor.getExtensionPages()) {
						System.out.println("\nPage Text: " + page.getPageText());
						if(page.getPageText().equals("Configuration Result")){
							ConfigurationResult cr = (ConfigurationResult)page;
							String[][] str = {{"Site", "ClientNR"}, {"Seq", "Concur", "File"}};
							//String[] s =  {"French", "Dutch"};
							cr.setResult(str);						
							//cr.selectFeature(tree.getTopItem(),s);						 
							cr.showResult();						
							setInput(configurationEditor.configuration);
						}
					}
					
				}
			}

			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
				// TODO Auto-generated method stub				
			}
		});
					
		tree = new Tree(parent, SWT.BORDER | SWT.CHECK);
		tree.addSelectionListener(new SelectionListener() {
			@Override
			public void widgetSelected(SelectionEvent event) {				
				if (event.detail == SWT.CHECK) {
					if ((((TreeItem)event.item).getText()).startsWith(configurationEditor.configuration.getRoot().getName())) {
						// case: root
						((TreeItem)event.item).setChecked(true);
						//((TreeItem)event.item).setGrayed(true);
					} else if (((TreeItem)event.item).getGrayed()) {
						// case: grayed and selected
						((TreeItem)event.item).setChecked(true);
					} else if (((TreeItem)event.item).getForeground().equals(gray)) {
						// case: grayed and unselected
						((TreeItem)event.item).setChecked(false);
					} else {
						// case: selectable
						if (!selectionChanged) {
							// do nothing if selection changed to fast
							if (((TreeItem)event.item).getChecked()) {
								((TreeItem)event.item).setChecked(true);								
							} else {
								((TreeItem)event.item).setChecked(false);
							}
						} else {
							changeSelection(configurationEditor.configuration.getSelectablefeature(
								((TreeItem)event.item).getText()));							
							refreshTree();
						}
					}
				}
			}
			
			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
			}
		});			  
	}

	@Override
	public void propertyChange(PropertyChangeEvent evt) {
		if (initialized)
			dirty = true;
		else
			initialized = true;
		UIJob job = new UIJob("refresh tree") {
			@Override
			public IStatus runInUIThread(IProgressMonitor monitor) {
				if (errorMessage()) {
					setInput(configurationEditor.configuration);			
				}
				return Status.OK_STATUS;
			}
		};
		job.setPriority(Job.SHORT);
		job.schedule();
	}

	private void refreshTree() {
		hiddenFeatures = new LinkedList<String>();
		for (Feature feature : configurationEditor.configuration.getFeatureModel().getFeatures()) {
			if (feature.isHidden())
				hiddenFeatures.add(feature.getName());
		}
		TreeItem root = tree.getItem(0);
		root.setText(AdvancedConfigurationLabelProvider.getRootlabel(configurationEditor.configuration));
		
		arlSelectedFeatures.clear();
		
		arlSelectedFeatures.add(configurationEditor.configuration.getRoot().getName().trim());
		setCheckbox(root);
	}

	private void setCheckbox(TreeItem root) {
		resetColor();
		setCheckbox(root, configurationEditor.configuration.valid());
		selectionChanged = true;
		setColor();
	}
	
	/**
	 * Stops the coloring job.
	 */
	public void resetColor() {
		returnFormThread = true;
		try {
			if (job_color != null) {
				job_color.join();
			}
		} catch (InterruptedException e) {
			FMUIPlugin.getDefault().logError(e);
		}
		items = new HashMap<SelectableFeature, TreeItem>();
		features = new LinkedList<SelectableFeature>();
	}
	
	private void setCheckbox(TreeItem item, boolean configuration_valid){		
		for (TreeItem child : item.getItems()) {
			child.setGrayed(false);
			child.setForeground(null);
			child.setBackground(null);
			child.setFont(treeItemStandardFont);
			SelectableFeature feature = configurationEditor.configuration.getSelectablefeature(child.getText());
			if (feature.getAutomatic() != Selection.UNDEFINED) {
				if (feature.getAutomatic() == Selection.SELECTED){
					child.setChecked(true);
					arlSelectedFeatures.add(feature.getName());
					child.setGrayed(true);
				} else {
					child.setChecked(false);
					child.setForeground(gray);
				}
			} else if (feature.getManual() == Selection.UNDEFINED || 
					feature.getManual() == Selection.UNSELECTED){
				child.setChecked(false);
				if(!configuration_valid) {
					features.add(feature);
					items.put(feature, child);
				}
			} else if (feature.getManual() == Selection.SELECTED) {
				child.setChecked(true);
				//System.out.print("\nFeature: " + feature.getName());
				arlSelectedFeatures.add(feature.getName());
				if(!configuration_valid) {
					features.add(feature);
					items.put(feature, child);
				}
			}					
			setCheckbox(child, configuration_valid);
		}		
	}
	
	/**
	 * Colors all features if they lead to a valid configuration 
	 * if current configuration is invalid. 
	 * deselect:blue, select:green 
	 */
	private void setColor() {
		returnFormThread = false;
		job_color = new Job("Feature coloring.(" + configurationEditor.file.getName() + ")") {
			public IStatus run(IProgressMonitor monitor) {
				if (features != null && features.size() != 0 && !features.isEmpty()) {
					monitor.beginTask("", features.size());
					for (SelectableFeature feature : features) {
						monitor.subTask("Check feature " + feature.getName());
						if (returnFormThread || monitor.isCanceled()) {
							monitor.done();
							return Status.OK_STATUS;
						}
						if (feature.getManual() == Selection.SELECTED) {
							if (configurationEditor.configuration.leadToValidConfiguration(feature, Selection.UNDEFINED, Selection.SELECTED )) {
								if (!returnFormThread) {
									setColor(items.get(feature), blue);
								}
							}
						} else {
							if (configurationEditor.configuration.leadToValidConfiguration(feature, Selection.SELECTED, Selection.UNDEFINED)) {
								if (!returnFormThread) {
									setColor(items.get(feature), green);
								}
							}
						}
						monitor.worked(1);
					}
				}
				monitor.done();
				return Status.OK_STATUS;
			}
		};
		job_color.setPriority(Job.DECORATE);
		job_color.schedule();
	}

	protected void setColor(final TreeItem item, final Color color) {
		UIJob job_setColor = new UIJob("") {
			@Override
			public IStatus runInUIThread(IProgressMonitor monitor) {
				item.setForeground(color);
				item.setFont(treeItemSpecialFont);
				return Status.OK_STATUS;
			}
		};
		job_setColor.setPriority(Job.SHORT);
		job_setColor.schedule();
	}

	private void setInput(Configuration configuration){
		hiddenFeatures = new LinkedList<String>();
		for (Feature feature : configuration.getFeatureModel().getFeatures()) {
			if (feature.isHidden())
				hiddenFeatures.add(feature.getName());
		}
		tree.removeAll();
		TreeItem item = new TreeItem(tree, 0);
		item.setText(AdvancedConfigurationLabelProvider.getRootlabel(configuration));
		add(item,configuration.getRoot().getChildren());
		setCheckbox(item);
		item.setGrayed(true);
		item.setExpanded(true);
		item.setChecked(true);
	}
	
	private void add(TreeItem parent,TreeElement[] children){
		for (TreeElement child : children){
			String childName = child.toString();
			if (!hiddenFeatures.contains(childName)) {
				TreeItem item = new TreeItem(parent,0);
				item.setText(childName);
				add(item,child.getChildren());
				item.setExpanded(true);
			}
		}
	}
	
	private boolean errorMessage() {

		if (configurationEditor.configuration==null||(!configurationEditor.configuration.valid() && configurationEditor.configuration.number() == 0)){
			tree.removeAll();
			TreeItem item = new TreeItem(tree, 1);
			if (configurationEditor.modelFile ==  null) {
				item.setText("There is no feature model corresponding to this configuration, reopen the editor and select one.");
			} else if (!configurationEditor.modelFile.exists()) {
				// This case should never happen
				item.setText("The given feature model " + configurationEditor.modelFile.getPath() + " does not exist.");
			} else {
				item.setText("The feature model for this project is void, i.e., " +
						"there is no valid configuration. You need to correct the " +
						"feature model before you can create or edit configurations.");
			}
			item.setImage(FMUIPlugin.getDefault()
					.getWorkbench().getSharedImages().getImage
					(ISharedImages.IMG_OBJS_ERROR_TSK));
			item.setChecked(true);
			item.setGrayed(true);
				dirty = false;
				return false;
		}
		return true;
	}
	
	protected void changeSelection(SelectableFeature feature) {
		selectionChanged = false;
		resetColor();
		
		if (feature.getAutomatic() == Selection.UNDEFINED) {
			// set to the next value
			if (feature.getManual() == Selection.UNDEFINED ||
					feature.getManual() == Selection.UNSELECTED)
				set(feature, Selection.SELECTED);
			else // case: selected
			{
				set(feature, Selection.UNDEFINED);				
			}
			
			if (!dirty) {
				dirty = true;
				firePropertyChange(IEditorPart.PROP_DIRTY);
			}
		}				
	}

	protected void set(SelectableFeature feature, Selection selection) {
		configurationEditor.configuration.setManual(feature, selection);
	}

	/* (non-Javadoc)
	 * @see de.ovgu.featureide.ui.editors.IConfigurationEditorPage#getPageText()
	 */
	@Override
	public String getPageText() {
		return PAGE_TEXT;
	}

	/* (non-Javadoc)
	 * @see de.ovgu.featureide.ui.editors.IConfigurationEditorPage#getID()
	 */
	@Override
	public String getID() {
		return ID;
	}

}