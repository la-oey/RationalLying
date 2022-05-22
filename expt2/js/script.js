// https://ucsd.sona-systems.com/webstudy_credit.aspx?experiment_id=1465&credit_token=c6393dd431374ab48035c7fafafced2e&survey_code=XXXX
// experiment settings
var expt = {
    saveURL: 'submit.simple.php',
    trials: 100, //switch to 100
    practiceTrials: 4, //how many practice trials //switch to 4
    //goalScore: 100,
    marblesSampled: 10, //total number of marbles drawn per trial
    roles: ['bullshitter', 'bullshitDetector'],
    roleFirst: 'bullshitter', //roles: {'bullshitter','bullshitDetector'}
    allTrialProbs: [0.2,0.5,0.8],
    occlude: ['computer','human'],
    catchTrials: [],
    // asymmTrials: [],
    pseudo: null,
    stat: {
        playerTotalScore: 0,
        oppTotalScore: 0,
        truth: 0,
        truth_noBS: 0,
        truth_BS: 0,
        lie: 0,
        lie_noBS: 0,
        lie_BS: 0,
        noBS: 0,
        noBS_truth: 0,
        noBS_lie: 0,
        BS: 0,
        BS_truth: 0,
        BS_lie: 0
    },
    sona: {
        experiment_id: 1505,
        credit_token: 'b20092f9d3b34a378ee654bcc50710ea'
    },
    debug: false
};
var trial = {
    exptPart: 'practice', //parts: {'practice','trial'}
    roleCurrent: 'bullshitter',
    trialNumber: 0,
    startTime: 0,
    trialTime: 0,
    waitTime: 0,
    responseStartTime: 0,
    responseTime: 0,
    timer: 0,
    prob: {
        bullshitterRed: 0.5,
        bullshitDetectorRed: 0.5,
    },
    asymm: true,
    marblesDrawn: [],
    numRed: 0,
    numBlue: 0,
    drawnRed: 0,
    drawnBlue: 0,
    reportedDrawn: 0,
    compLie: 0,
    compUnifLie: false,
    compDetect: 0,
    callBS: false,
    callBStxt: '',
    catch: {
        question: '',
        response: 0,
        key: 0,
        responseStartTime: 0,
        responseTime: 0
    },
    pseudoRound: false,
    playerTrialScore: 0,
    oppTrialScore: 0
};
var turn = {
    numDrawn: 0
}
var client = parseClient();
var trialData = []; // store of all trials



// TODO, Potentially: pick randomly between human/threePoints instructions.
function pageLoad() {
    document.getElementById('consent').style.display = 'block';
}

function clickConsent() {
    document.getElementById('consent').style.display = 'none';
    document.getElementById('instructions').style.display = 'block';
    $('#instructPractice').html(expt.practiceTrials);
    $('#instructRounds').html(expt.trials);
    $('#instructMarblesSampled').html(expt.marblesSampled);
    expt.occlude = sample(expt.occlude);
}

function clickInstructions() {
    document.getElementById('instructions').style.display = 'none';
    document.getElementById('prePractice').style.display = 'block';
}

function clickPrePractice(){
    document.getElementById('prePractice').style.display = 'none';
    expt.catchTrials = distributeChecks(expt.practiceTrials, 1); // 50% of practice trials have an attention check
    if(expt.roleFirst == 'bullshitter'){
        bullshitter();
    } else{
        bullshitDetector();
    }
}

function clickPostPractice(){
    document.getElementById('postPractice').style.display = 'none';

    expt.catchTrials = distributeChecks(expt.trials, 0.15); // 0.1 of expt trials have an attention check
    
    expt.pseudo = distributePseudo(expt.trials, 0, 10);

    //console.log(expt.pseudo);
    expt.roleFirst = sample(expt.roles);
    //expt.asymmTrials = distributeAsymm(expt.trials, 0.4);
    trial.roleCurrent = expt.roleFirst;
    if(expt.roleFirst == 'bullshitter'){
        bullshitter();
    } else{
        bullshitDetector();
    }
}




function bullshitter() {
    restartTrial();

    var marbleInstruct = "<p class='instructText'>Click the 'Draw Marble' button to sample marbles from the box. Draw <b>10</b> marbles.</p>";

    $('#trialInstruct').html(marbleInstruct);
    var responseText = "<p><br>Each <b style='color:red'>red</b> you report is 1 point for you; each <b style='color:blue'>blue</b> is 1 point for your opponent.</p>";
    responseText += "<label><br>Say how many <b style='color:red'>red</b> marbles you want your opponent to think you drew:</label>";
    responseText += "<input type='text' id='reportMarbles' value='' size='2' maxlength='2' autocomplete='off' autocorrect='off' autocapitalize='off' spellcheck='false'/> ";
    responseText += "<button class='active-button' id='report-button' type='button' onclick='report();'>Report!</button><br><br>";
    $('#subjResponse').html(responseText);
    $('#urnsvg').css('background-color','white');
    $('#tubesvg').css('background-color','white');
    $('#draw-button').prop('disabled',false);
    $('#report-button').prop('disabled',true);
    $('input[type=text]').on('input',
        function(){
            trial.reportedDrawn = parseInt($(this).val());
            if(trial.reportedDrawn >= 0 && trial.reportedDrawn <= 10 ){
                $('#report-button').prop('disabled',false);
            } else{
                $('#report-button').prop('disabled',true);
            }
    });
    
}



function bullshitDetector() {
    restartTrial();

    $('#trialInstruct').html("");
    $('#tubesvg').css('background-color','purple');
    $('#draw-button').prop('disabled',true);

    function bullshitDetectWait() {
        flickerWait();
        
        trial.waitTime = 3000 + 6000*exponential(0.75);
        setTimeout(function(){
            clearInterval(trial.timer);

            var responseInstruct = "<p>Your opponent said they drew <b id='reportMarbles'/> red marbles.</p>"
            responseInstruct += "<p>Your opponent will win <b id='oppPoints'></b> points and you will win <b id='yourPoints'/> points this round.<br><br></p>";
            responseInstruct += "<p id='responseAccRej'>Click <b style='color:green'>'Accept'</b> if you think your opponent is <b style='color:green'>telling the truth</b>, or <b style='color:red'>'Reject'</b> if you think your opponent is <b style='color:red'>lying</b>.</p>"
            
            $('#subjResponse').html(responseInstruct);
            $('#subjResponse').css('opacity','1');
            if(trial.exptPart == "trial"){
                $('#responseAccRej').css('opacity','0');
                setTimeout(function(){
                    $('#responseAccRej').css('opacity','1');
                }, 5000);    
            }
            
            computerDraw();
            $('#reportMarbles').html(trial.reportedDrawn);
            $('#oppPoints').html(trial.reportedDrawn);
            $('#yourPoints').html(expt.marblesSampled - trial.reportedDrawn);
            
            $('#buttonResponse').css('opacity','1');
            trial.responseStartTime = Date.now();
        }, trial.waitTime);
    }
    bullshitDetectWait();
}



function toScoreboard(){
    document.getElementById('trial').style.display = 'none';
    document.getElementById('scoreboard').style.display = 'block';

    trial.catch.responseStartTime = Date.now();
    if(expt.catchTrials.includes(trial.trialNumber)){
        $('#catchQ').show();
        catchTrial(trial.roleCurrent, trial.exptPart);
    } else if(trial.exptPart == 'trial' & (trial.trialNumber + 1) % 5 != 0){
        $('#totalScoreboardDiv').css('opacity','0');
    } else{
        $('#totalScoreboardDiv').css('opacity','1');
    }

    if(trial.roleCurrent == 'bullshitter'){
        computerBSDetector();
    }

    if(!trial.callBS){
        if(trial.roleCurrent == 'bullshitter'){
            trial.callBStxt = "Your opponent <b style='color:green'>accepted</b> your reported answer.<br><br>";
            trial.playerTrialScore = trial.reportedDrawn; 
            trial.oppTrialScore = expt.marblesSampled - trial.reportedDrawn;
            if(trial.reportedDrawn == trial.drawnRed){
                trial.callBStxt = trial.callBStxt + "You were <b>telling the truth</b>.<br><br>";
                expt.stat.truth += 1;
                expt.stat.truth_noBS += 1;
            } else{
                trial.callBStxt = trial.callBStxt + "You were <b>lying</b>.<br>The true answer was <b>" + trial.drawnRed + "</b>.";
                expt.stat.lie += 1;
                expt.stat.lie_noBS += 1;
            }
        } else{
            trial.callBStxt = "You <b style='color:green'>accepted</b> your opponent's reported answer.<br><br>";
            expt.stat.noBS += 1;
            trial.oppTrialScore = trial.reportedDrawn;
            trial.playerTrialScore = expt.marblesSampled - trial.reportedDrawn;
            if(trial.reportedDrawn == trial.drawnRed){
                trial.callBStxt = trial.callBStxt + "Your opponent was <b>telling the truth</b>.<br><br>";
                expt.stat.noBS_truth += 1;
            } else{
                trial.callBStxt = trial.callBStxt + "Your opponent was <b>lying</b>.<br>The true answer was <b>" + trial.drawnRed + "</b>.";
                expt.stat.noBS_lie += 1;
            }
        }
    } else{
        if(trial.roleCurrent == 'bullshitter'){
            trial.callBStxt = "Your opponent <b style='color:red'>rejected</b> your reported answer.<br><br>";
            //if player is telling the truth
            if(trial.reportedDrawn == trial.drawnRed){
                trial.callBStxt = trial.callBStxt + "You were <b>telling the truth</b>.<br><br>";
                expt.stat.truth += 1;
                expt.stat.truth_BS += 1;
                trial.playerTrialScore = trial.reportedDrawn; //opponent gets 10 - points as reported w/ -5 penalty
                trial.oppTrialScore = expt.marblesSampled - trial.reportedDrawn - 5; //player gets points as reported

            } else{
                trial.callBStxt = trial.callBStxt + "You were <b>lying</b>.<br>The true answer was <b>" + trial.drawnRed + "</b>.";
                expt.stat.lie += 1;
                expt.stat.lie_BS += 1;
                trial.playerTrialScore = -5; //player gets -5 points
                trial.oppTrialScore = 5; //opponent gets +5 points
            }
        } else{
            trial.callBStxt = "You <b style='color:red'>rejected</b> your opponent's reported answer.<br><br>";
            expt.stat.BS += 1;
            //if player catches a liar
            if(trial.reportedDrawn == trial.drawnRed){
                trial.callBStxt = trial.callBStxt + "Your opponent was <b>telling the truth</b>.<br><br>";
                expt.stat.BS_truth += 1;
                trial.playerTrialScore = expt.marblesSampled - trial.reportedDrawn - 5; //player gets 10 - points as reported w/ -5 penalty
                trial.oppTrialScore = trial.reportedDrawn; //opponent gets points as reported
            } else{
                trial.callBStxt = trial.callBStxt + "Your opponent was <b>lying</b>.<br>The true answer was <b>" + trial.drawnRed + "</b>.";
                expt.stat.BS_lie += 1;
                trial.playerTrialScore = 5; //player gets +5 points
                trial.oppTrialScore = -5; //opponent gets -5 points
            }
        }
    }

    expt.stat.playerTotalScore += trial.playerTrialScore;
    expt.stat.oppTotalScore += trial.oppTrialScore;
    $('.playerScore').html(expt.stat.playerTotalScore);
    $('.oppScore').html(expt.stat.oppTotalScore);

    if(trial.exptPart == "practice"){
        $('#calledBS').html(trial.callBStxt);
        $('#playerPts').html(scorePrefix(trial.playerTrialScore));
        $('#oppPts').html(scorePrefix(trial.oppTrialScore));
        //$('.playerScore').html((expt.stat.playerTotalScore - trial.playerTrialScore) + " + " + trial.playerTrialScore + " = " + expt.stat.playerTotalScore);
        //$('.oppScore').html((expt.stat.oppTotalScore - trial.oppTrialScore) + " + " + trial.oppTrialScore + " = " + expt.stat.oppTotalScore);
    } else{
        $('.scoreReport').html("Click to move on to the next round.");
        $('#trialScoreboardDiv').hide();
    }
}

function trialDone() {
    // hide trial.
    document.getElementById('scoreboard').style.display = 'none';
    trial.trialTime = Date.now() - trial.startTime;
    trial.trialNumber += 1;
    recordData();

    if(trial.exptPart == "practice" & trial.trialNumber >= expt.practiceTrials){
        trial.trialNumber = 0;
        trial.exptPart = 'trial';
        expt.stat.playerTotalScore = 0;
        expt.stat.oppTotalScore = 0;
        expt.stat.truth = 0;
        expt.stat.truth_noBS = 0;
        expt.stat.truth_BS = 0;
        expt.stat.lie = 0;
        expt.stat.lie_noBS = 0;
        expt.stat.lie_BS = 0;
        expt.stat.noBS = 0;
        expt.stat.noBS_truth = 0;
        expt.stat.noBS_lie = 0;
        expt.stat.BS = 0;
        expt.stat.BS_truth = 0;
        expt.stat.BS_lie = 0;
        document.getElementById('trial').style.display = 'none';
        document.getElementById('postPractice').style.display = 'block';
    } else if(trial.trialNumber >= expt.trials){
        if(expt.stat.playerTotalScore == expt.stat.oppTotalScore){
            $('#whowon').html("You and your opponent tied!");
        } else if(expt.stat.playerTotalScore > expt.stat.oppTotalScore){
            $('#whowon').html("You won!");
        } else{
            $('#whowon').html("Your opponent won!");
        }

        $('.scoreboardDiv').show();

        $('.playerScore').html(expt.stat.playerTotalScore);
        $('.oppScore').html(expt.stat.oppTotalScore);

        calculateStats('#stat_truth', expt.stat.truth, expt.stat.truth + expt.stat.lie);
        calculateStats('#stat_truth_noBS', expt.stat.truth_noBS, expt.stat.truth);
        calculateStats('#stat_truth_BS', expt.stat.truth_BS, expt.stat.truth);
        calculateStats('#stat_lie', expt.stat.lie, expt.stat.truth + expt.stat.lie);
        calculateStats('#stat_lie_noBS', expt.stat.lie_noBS, expt.stat.lie);
        calculateStats('#stat_lie_BS', expt.stat.lie_BS, expt.stat.lie);
        calculateStats('#stat_noBS', expt.stat.noBS, expt.stat.noBS + expt.stat.BS);
        calculateStats('#stat_noBS_truth', expt.stat.noBS_truth, expt.stat.noBS);
        calculateStats('#stat_noBS_lie', expt.stat.noBS_lie, expt.stat.noBS);
        calculateStats('#stat_BS', expt.stat.BS, expt.stat.noBS + expt.stat.BS);
        calculateStats('#stat_BS_truth', expt.stat.BS_truth, expt.stat.BS);
        calculateStats('#stat_BS_lie', expt.stat.BS_lie, expt.stat.BS);

        // expt done
        data = {client: client, expt: expt, trials: trialData};
        writeServer(data);

        document.getElementById('completed').style.display = 'block';
    } else {
        if(trial.roleCurrent == 'bullshitter'){
            trial.roleCurrent = 'bullshitDetector';
            bullshitDetector();
        } else{
            trial.roleCurrent = 'bullshitter';
            bullshitter();
        }
    }
}


function experimentDone() {
    submitExternal(client);
}

