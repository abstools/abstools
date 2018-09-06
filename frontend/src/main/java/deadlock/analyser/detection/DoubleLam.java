package deadlock.analyser.detection;

//This Class is only create to easily pass a pair of Lamps, only getter and setter method are provided
//N.B. do not confuse BoubleLam with BigLam, the second one is more important and conserve main informations
public class DoubleLam {

    // the only field are the 2 lams
    Lam w;
    Lam wPrime;

    //constructor
    public DoubleLam(){
        this.w = new Lam();
        this.wPrime = new Lam();
    }


    //getter and setter

    public Lam getW(){
        return this.w;
    }

    public Lam getWPrime(){
        return this.wPrime;
    }

    public void setW(Lam newW){
        this.w = newW;
    }

    public void setWPrime(Lam newW){
        this.wPrime = newW;
    }

    //union
    public void union(DoubleLam dl1, DoubleLam dl2){
        Lam lAus = new Lam();
        lAus.addLamp(dl1.getW());
        lAus.addLamp(dl2.getW());

        this.w = lAus;

        Lam lAus2 = new Lam();
        lAus2.addLamp(dl1.getWPrime());
        lAus2.addLamp(dl2.getWPrime());

        this.wPrime = lAus2;
    }

    //sequence composition
    public void seqComposition(DoubleLam dl){
        if(dl.getW().getStates().isEmpty()){
            this.wPrime.parallel(dl.getWPrime());
            return;
        }
        Lam lAus = new Lam();
        lAus.addLamp(dl.getW());
        lAus.parallel(this.wPrime);
        this.w.addLamp(lAus);

        this.wPrime.parallel(dl.getWPrime());
    }


    public void parallel(DoubleLam dl1, DoubleLam dl2) {
        
        Lam lAus = new Lam();
        lAus.addLamp(dl1.getW());
        lAus.addLamp(dl1.getWPrime());

        
        Lam lAus2 = new Lam();
        lAus2.addLamp(dl2.getW());
        lAus2.addLamp(dl2.getWPrime());

        lAus.parallel(lAus2);
        
        this.w = lAus; 
        this.wPrime = new Lam();
    }


    public void updateStackTrace(String method) {
       this.w.updateStackTrace(method);
       this.w.updateStackTrace(method);
    }

  
}
