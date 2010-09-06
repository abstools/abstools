package abs.backend.java.visualization;

import java.util.Arrays;
import java.util.List;

public class CoCoME extends Main {
	private final List<String> environmentClasses = Arrays.asList(
            "CashBoxEnvImpl",
            "CardReaderEnvImpl",
            "ScreenImpl",
            "PrinterEnvImpl",
            "InventoryImpl",
            "BankImpl",
            "BarCodeScannerEnvImpl"
            );
    private final List<String> systemClasses = Arrays.asList(
            "CashBoxImpl",
            "CashDeskPCImpl",
            "CardReaderImpl",
            "PrinterImpl",
            "BarCodeScannerImpl",
            "ExpressCoordinatorImpl"
            );
    private final List<String> observedClasses = Arrays.asList(
            "CashBoxEnvImpl",
            "CardReaderEnvImpl",
            "InventoryImpl",
            "BarCodeScannerEnvImpl",
            //"ExpressCoordinatorImpl",
            "ScreenImpl",
            //"PrinterEnvImpl",
            "BankImpl",
            "CashBoxImpl",
            "CashDeskPCImpl",
            "BarCodeScannerImpl",
            "CardReaderImpl"
            //"PrinterImpl"
            );
    
    {
   	 abstractEnvironment = true;
   	 showStartMsg = false;
   	 staticActors = true;
    }

    @Override
    protected String getName() {
    	return "TradingSystem";
    }
    
    @Override
    protected void initializeActors() {
    	super.initializeActors();
        //out.println("#![Process Sale]");

        for (String s : getObservedClasses()) {
            if (!abstractEnvironment && getEnvironmentClasses().contains(s)) {
           		out.println(s+"_1:"+s+"[ap]");
            }
            if (getSystemClasses().contains(s)) {
           		out.println(s+"_1:"+s+"[a]");
            }
        }

        out.println();
    }
    
    @Override
    List<String> getObservedClasses() {
    	return observedClasses;
    }

    @Override
    List<String> getSystemClasses() {
    	return systemClasses;
    }

    @Override
    List<String> getEnvironmentClasses() {
    	return environmentClasses;
    }
}
