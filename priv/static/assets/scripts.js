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
    let sel = logTable.selectAll("tr").remove().data(log.messages);
    let messages = sel.enter().append("tr");
    messages.append("td").text(m => map.get(m.from).label)
        .classed("leader", m => log.leader === m.from)
        .classed("traitor", m => map.get(m.from).traitor);
    messages.append("td").text(m => map.get(m.to).label)
        .classed("leader", m => log.leader === m.to)
        .classed("traitor", m => map.get(m.to).traitor);
    messages.append("td").text(m => m.msg);
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