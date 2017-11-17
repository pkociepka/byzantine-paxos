function errorAlert() {
    return $('<div class="alert alert-danger alert-dismissible fade in" role="alert">\n    An error has occurred.\n    <button type="button" class="close" data-dismiss="alert" aria-label="Close">\n        <span aria-hidden="true">&times;</span>\n    </button>\n</div>');
}

function successAlert() {
    return $('<div class="alert alert-success alert-dismissible fade in" role="alert">\n    Successfully saved.\n    <button type="button" class="close" data-dismiss="alert" aria-label="Close">\n        <span aria-hidden="true">&times;</span>\n    </button>\n</div>');
}

function rejectedAlert() {
    return $('<div class="alert alert-warn alert-dismissible fade in" role="alert">\n    Save rejected.\n    <button type="button" class="close" data-dismiss="alert" aria-label="Close">\n        <span aria-hidden="true">&times;</span>\n    </button>\n</div>');
}

function showAlert(alert, container) {
    alert.appendTo(container).alert().addClass("show");
    setTimeout(() => alert.alert('close'), 3000)
}

const logTable = d3.select("#messageLog tbody");

function dumpLog(log) {
    let acceptors = log.acceptors.map((a, i) => ({pid: a, label: "A" + i, traitor: false}));
    let traitors = log.traitors.map((a, i) => ({pid: a, label: "T" + i, traitor: true}));
    let map = d3.map(acceptors.concat(traitors), e => e.pid);
    map.set(log.leader, {pid:log.leader, label:"Leader", traitor:false});
    let sel = logTable.selectAll("tr").data(log.messages);

    sel.exit().remove();

    let added = sel.enter().append("tr");
    added.append("td").attr("class", "from");
    added.append("td").attr("class", "to");
    added.append("td").attr("class", "msg");
    sel = added.merge(sel);

    function label(m) {
        let a = map.get(m);
        return a ? a.pid : m;
    }
    function isTraitor(m) {
        let a = map.get(m);
        return a && a.traitor;
    }

    sel.select(".from").text(m => label(m.from))
        .classed("leader", m => log.leader === m.from)
        .classed("traitor", m => isTraitor(m.from));
    sel.select(".to").text(m => label(m.to))
        .classed("leader", m => log.leader === m.to)
        .classed("traitor", m => isTraitor(m.to));
    sel.select(".msg").text(m => m.msg);


}

(function () {
    const readKey = $('#readKey'), readValue = $("#readValue"), readForm = $('#readForm');
    const readAlert = readForm.find('.alert-container');

    readForm.submit(e => {
        const key = readKey.val();
        $.get('/get/' + key).then(v => {
            readValue.val(v.value);
            dumpLog(v);
        }, v => {
            console.error(v);
            showAlert(errorAlert(), readAlert);
        });
        e.preventDefault()
    });


    const writeForm = $('#writeForm'), writeKey = $('#writeKey'), writeValue = $("#writeValue");
    const writeAlert = writeForm.find('.alert-container');

    writeForm.submit(e => {
        const key = writeKey.val(),
            newVal = writeValue.val();
        $.get('/put/' + key + "/" + newVal).then(v => {
            if (v.value) showAlert(successAlert(), writeAlert);
            else showAlert(rejectedAlert(), writeAlert);
            dumpLog(v);
        }, v => {
            console.error(v);
            showAlert(errorAlert(), writeAlert);
        });
        e.preventDefault()
    });

})();