package deadlock.analyser.detection;

//This Class is only create to easily pass a pair of Lamps, only getter and setter method are provided
//N.B. do not confuse BoubleLamp with BigLamp, the second one is more important and conserve main informations
public class DoubleLamp {

    // the only field are the 2 lamps
    Lamp w;
    Lamp wPrime;

    //constructor
    public DoubleLamp(){
        this.w = new Lamp();
        this.wPrime = new Lamp();
    }


    //getter and setter

    public Lamp getW(){
        return this.w;
    }

    public Lamp getWPrime(){
        return this.wPrime;
    }

    public void setW(Lamp newW){
        this.w = newW;
    }

    public void setWPrime(Lamp newW){
        this.wPrime = newW;
    }

    //union
    public void Union(DoubleLamp dl1, DoubleLamp dl2){
        Lamp lAus = new Lamp();
        lAus.addLamp(dl1.getW());
        lAus.addLamp(dl2.getW());

        this.w = lAus;

        Lamp lAus2 = new Lamp();
        lAus2.addLamp(dl1.getWPrime());
        lAus2.addLamp(dl2.getWPrime());

        this.wPrime = lAus2;
    }

    //sequence composition
    public void seqComposition(DoubleLamp dl){
        if(dl.getW().getStates().isEmpty()){
            this.wPrime.parallel(dl.getWPrime());
            return;
        }
        Lamp lAus = new Lamp();
        lAus.addLamp(dl.getW());
        lAus.parallel(this.wPrime);
        this.w.addLamp(lAus);

        this.wPrime.parallel(dl.getWPrime());
    }

  
}
