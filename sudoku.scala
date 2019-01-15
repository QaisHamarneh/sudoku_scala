object Window {
    import scala.swing._
    import scala.language.postfixOps._
    def main(args:Array[String]):Unit ={
        
        
        mainWindow.visible = true   
    }


case class Box(val horizental:Int , val vertical:Int  , var value:Int = 0 , var valueOptions:List[Int] = List(1 ,2, 3 ,4 ,5 ,6,7 ,8 ,9)){
        require(value >= 0 && value <= 9 , "some other exception")
        //assert(!valueOptions.contains(0) , "illegal option 0")
        //assert(valueOptions.length != 0 , "no more options")
        val boxNr = horizental * 9 + vertical
        val square = (horizental/3) * 3 + vertical/3

        def sameBox(that:Box):Boolean = {
            this.vertical == that.vertical &&
            this.horizental == that.horizental 
        }
        
        def value_(newValue:Int):Boolean ={
            if (newValue == 0) {
                //println("setting the value to 0")
                return false
            }
            else if (this.value != 0) {
                //println("changing an exsiting value")
                return false
            }
            else if(!valueOptions.contains(newValue)) {
                //println("adding a value outside the value options")
                return false
            }
            else{
                value = newValue
                valueOptions = valueOptions.filter(_ == newValue)
                return true
            }
        }
        def sameOptions(that:Box) ={
            this.valueOptions.forall(o => that.valueOptions.contains(o)) &&
            this.valueOptions.length == that.valueOptions.length
        }
        override def toString ={
            horizental + " " + vertical + "  " + 
            value + "   	"
        }
}

class Matrix(var lines: Array[Array[Box]]) {
    //require (lines.length == lines(0).length , "not a square Matrix")
    def this() =  this(Array.ofDim[Box](9 , 9))
    val n = lines.length
    
    def fillTheMatrix:Unit ={
        for (i <- 0 until n){
            for(j <- 0 until n){
                lines(i)(j) = Box(i , j)
            }
        } 
    }
    fillTheMatrix
    
    override def toString ={
        var result = ""
        for (line <- lines){
            for (box <- line){
               result +=  box.toString
            }
            result += "\n"
        }
        result
    }
    
    
    val matrixArray:Array[Box] = new Array[Box] (n * n)
        var indexing = 0
    for (line <- lines){
        for (box <- line){
            matrixArray(indexing) = box
            indexing += 1
        }
    }
    
    
    val verticalArray = (box:Box) => matrixArray.filter(b => b.vertical == box.vertical)
    val squareArray = (box:Box) => matrixArray.filter(b => b.square == box.square)
    
    
    def commonBoxes(box:Box):Array[Box] ={
        (squareArray(box) ++ 
            verticalArray(box) ++ 
                lines(box.horizental)).filter(!box.sameBox(_))
    }
    
    def safeOptions(box:Box):Unit ={
        val common = commonBoxes(box).map(b => b.value)
        for( i <- box.valueOptions){
            if (common.contains(i)) box.valueOptions = box.valueOptions.filter(_ != i)
        }
        if(box.valueOptions.length == 1) box.value_(box.valueOptions(0))
    }
    
    
    def keepSafeOptions ={
        for (box <- matrixArray if box.value == 0){
            safeOptions(box)
        }
        for (box <- matrixArray) 
            if(box.valueOptions.length == 1 && box.value == 0)
                box.value_(box.valueOptions(0))
    }
    
    def done :Boolean ={
        lines.forall(line => !line.map(box =>box.value).contains(0))
    }
    
    def keepTheOnlyOptions ={
        def onlyOptionInList(number:Int , box:Box):Boolean = {
               lines(box.horizental).filter(_.vertical != box.vertical).forall(b => !b.valueOptions.contains(number)) ||
               verticalArray(box).filter(_.horizental != box.horizental).forall(b => !b.valueOptions.contains(number)) ||
               squareArray(box).filter(!_.sameBox(box)).forall(b => !b.valueOptions.contains(number))
        }
    
        for (box <- matrixArray if box.value == 0){
            for (i <- box.valueOptions){
                //if(commonBoxes(box).map(b => b.value).contains(i)) println("vaiolation " + box + box.valueOptions + " " + i)
                if(onlyOptionInList(i, box)) box.value_(i)
            }
        }
    }
    
    def equalMatrix(that:Matrix):Boolean ={
        for (l <- 0 until n){
            for (b <- 0 until n){
                if(this.lines(l)(b).value != that.lines(l)(b).value) {
                    //println(this.lines(l)(b).toString + that.lines(l)(b).toString)
                    return false
                }
                if (!this.lines(l)(b).sameOptions(that.lines(l)(b))){
                    //println(this.lines(l)(b).toString + that.lines(l)(b).toString)
                    //println(this.lines(l)(b).valueOptions + "  " + that.lines(l)(b).valueOptions)
                    return false
                }
            }
        }
        //println("they were equal TRUE")
        return true
    }
    
    def makeACopy:Matrix ={
        val copy = new Matrix(Array.ofDim[Box](n , n))
        for (l <- 0 until this.matrixArray.length){
                copy.matrixArray(l).value = this.matrixArray(l).value
                copy.matrixArray(l).valueOptions = copy.matrixArray(l).valueOptions.filter(this.matrixArray(l).valueOptions.contains(_))
        }
        copy
    }
    
    def valiolation:Boolean ={
        for (line <- lines){
            for (box <- line){
                if(box.valueOptions.length == 0) {
                    return true
                }
                val common = commonBoxes(box).map(b => b.value).filter(_ != 0)
                if(common.contains(box.value)){
                    return true
                }
            }
        }
        return false
    }
    
    
    def preemitivSet(box:Box , range:Array[Box]) ={
        if(box.valueOptions.length >= 2 && box.valueOptions.length <= 6){
        val theBoxesSet = range.filter(b => b.valueOptions.forall(o => box.valueOptions.contains(o)))
        var theRest:Array[Box] = new Array(0)
        if (theBoxesSet.length == box.valueOptions.length) {
            theRest = range.filter(b => !theBoxesSet.contains(b))}
        theRest.foreach(b => b.valueOptions = b.valueOptions.filter(!box.valueOptions.contains(_)))
        for (box <- range) 
            if(box.valueOptions.length == 1 && box.value == 0) 
                box.value_(box.valueOptions(0))
        }
    }
    
    def eleminatePreemitivSet ={
        for (box <- matrixArray){
            preemitivSet(box , lines(box.horizental))
            preemitivSet(box , verticalArray(box))
            preemitivSet(box , squareArray(box))
        }
        for (box <- matrixArray) 
            if(box.valueOptions.length == 1 && box.value == 0) 
                box.value_(box.valueOptions(0))
    }
    
    var answer:Option[Matrix] = None
    
    def solve:Unit ={
        var stop = false
        var stuck1 = false
        //var mal = 0
        //var copy = this.makeACopy
        while(!this.done  && !stuck1){ // && mal <= 200){
            val copy:Matrix = this.makeACopy
            this.keepSafeOptions
            this.keepTheOnlyOptions
            this.eleminatePreemitivSet
            if(this.equalMatrix(copy)) stuck1 = true
            //mal += 1
        }
        if(stuck1 && !this.done){ // && mal > 200 ||
            for (l <- 0 until n if !stop){
                for(b <- 0 until n if this.lines(l)(b).value == 0 && !stop){
                    for(option <- this.lines(l)(b).valueOptions  if !stop){
                        val copy = this.makeACopy
                        copy.lines(l)(b).value_(option)
                        var cStuck = false
                        //mal = 0
                        while(!copy.done && !cStuck & !copy.valiolation){ //&& mal <= 200){
                            val copyOfCopy = copy.makeACopy
                            copy.keepSafeOptions
                            copy.keepTheOnlyOptions
                            copy.eleminatePreemitivSet
                            if(copy.equalMatrix(copyOfCopy)) cStuck = true
                            //mal += 1
                        }
                        if(copy.done){
                            answer = Some(copy)
                            stop = true
                        }
                    }
                }
            }
        }
    }
    

}


    
    var soduko = new Matrix
    val mainWindow = new MainFrame{
        title = "solve sudoku"
        location = new Point(550 , 150)
    }

    var flowList:List[FlowPanel] = Nil
    
    for (i <- 0 until 9 reverse){
        
        var txFdList:List[TextField] = Nil
        var line = new FlowPanel
        
        for (j <- 0 until 9 reverse){
            
            var txFd = new TextField
            txFd.columns = 2
            txFdList ::= txFd
        }
        line.contents ++= txFdList
        flowList ::= line
    }
    
    val resultsLabel = new Label
    resultsLabel.text = "                 "
    
    val solveAction = new Action("solve"){
        def apply() {
            for ( f <- 0 until flowList.length) {
                for (c <- 0 until flowList(f).contents.length){
                    flowList(f).contents(c) match {
                        case tx:TextField => { 
                            var y = 0
                            if (tx.text == "") y = 0 
                            else {
                                try {
                                    y = tx.text.toInt
                                } catch {
                                    case notNumber:NumberFormatException => {
                                        resultsLabel.text = "Please enter only numbers between 1 and 9"
                                        return
                                    }
                                    case ex:Exception => 10
                                }
                            }
                            if (y > 9 || y < 0) {
                                resultsLabel.text = "Please enter only numbers between 1 and 9"
                                return
                            }
                            else if (y != 0) soduko.lines(f)(c).value_(y)
                        }
                    }       
                }
            }
            soduko.solve 
            val answer = soduko.answer match {
                case None => soduko
                case Some(ans) => ans
            }
            if(!answer.done) resultsLabel.text = "Not enough numbers to solve the sudoku"
            for ( f <- 0 until flowList.length)  {
                for (c <- 0 until flowList(f).contents.length){
                    flowList(f).contents(c) match {
                        case tx:TextField => { 
                            tx.text = answer.lines(f)(c).value.toString
                        }
                    }   
                }
            }
        }
    }
    
    
    val clearAction = new Action("clear"){
        def apply() {
            for ( f <- 0 until flowList.length)  {
                for (c <- 0 until flowList(f).contents.length){
                    flowList(f).contents(c) match {
                        case tx:TextField => { 
                            tx.text = ""
                        }
                    }   
                }
            }
        }
    }
    
    val resultFlow = new FlowPanel
    resultFlow.contents ++= Seq(resultsLabel) 
    val buttonFlow = new FlowPanel
    val solveButton = new Button(solveAction)
    val clearButton = new Button(clearAction)
    buttonFlow.contents += clearButton
    buttonFlow.contents += solveButton
    
    
    val panelBox = new BoxPanel(Orientation.Vertical)
    panelBox.contents ++= flowList
    panelBox.contents += resultFlow
    panelBox.contents += buttonFlow
    
    mainWindow.contents = panelBox
    
}
